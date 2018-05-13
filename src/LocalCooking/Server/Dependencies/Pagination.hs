{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , GADTs
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , RankNTypes
  , ScopedTypeVariables
  #-}

module LocalCooking.Server.Dependencies.Pagination where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Singleton.Class (Extractable (..))
import Data.Insert.Class (Insertable)
import qualified Data.Insert.Class as Insert
import Control.Applicative (Alternative (empty))
import Control.Monad (forM_, forM, forever)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Trans.Control.Aligned as Aligned
import Control.Concurrent.Async (Async, cancel, async)
import Control.Concurrent.STM (STM, atomically, TVar, newTVarIO, readTVar, writeTVar, modifyTVar, TChan, newTChanIO, writeTChan, readTChan)
import Database.Persist.Sql (runSqlPool, SqlBackend)
import Database.Persist.Class (PersistEntity (Key, EntityField, PersistEntityBackend), selectList, insert, replace, delete)
import qualified Database.Persist.Types as Persist
import Web.Dependencies.Sparrow (Server, ServerArgs (..), ServerContinue (..), ServerReturn (..))


data SortOrdering = Asc | Dsc

data PaginationArgs fieldLabel = PaginationArgs
  { paginationArgsField         :: fieldLabel
  , paginationArgsFieldOrdering :: SortOrdering
  , paginationArgsPageSize      :: Int
  , paginationArgsPageIndex     :: Int
  }


paginationArgsToQuery :: PaginationArgs (EntityField record typ) -> [Persist.SelectOpt record]
paginationArgsToQuery PaginationArgs{..} =
  [ case paginationArgsFieldOrdering of
      Asc -> Persist.Asc paginationArgsField
      Dsc -> Persist.Desc paginationArgsField
  , Persist.LimitTo paginationArgsPageSize
  , Persist.OffsetBy (paginationArgsPageIndex * paginationArgsPageSize)
  ]


newtype PaginationInitIn fieldLabel = PaginationInitIn (PaginationArgs fieldLabel)

newtype PaginationInitOut a = PaginationInitOut [a]

data PaginationDeltaIn fieldLabel
  = PaginationChangeSize Int
  | PaginationChangeIndex Int
  | PaginationResort fieldLabel SortOrdering

data PaginationDeltaOut a
  = PaginationUpdate a
  | PaginationFlush [a]


-- type ObservationScope record =
--   HashSet (Key record)


newtype ListenerId = ListenerId
  { getListenerId :: UUID
  } deriving (Eq, Hashable, Show)

data ListenerMsg record
  = Update record
  | Delete

type Listeners record = HashMap (Key record) (HashMap ListenerId (TChan (ListenerMsg record)))

registerListener :: Eq (Key record)
                 => Hashable (Key record)
                 => TVar (Listeners record)
                 -> Key record
                 -> TChan (ListenerMsg record)
                 -> IO ListenerId
registerListener listeners recordKey chan = do
  key <- ListenerId <$> nextRandom
  atomically $ modifyTVar listeners $
    HashMap.alter
      (maybe (Just (HashMap.singleton key chan)) (Just . HashMap.insert key chan))
      recordKey
  pure key

unregisterListener :: Eq (Key record)
                   => Hashable (Key record)
                   => TVar (Listeners record)
                   -> Key record
                   -> ListenerId
                   -> STM ()
unregisterListener listeners recordKey key =
  modifyTVar listeners $
    HashMap.update
      (prune . HashMap.delete key)
      recordKey
  where
    prune xs
      | HashMap.null xs = Nothing
      | otherwise = Just xs

insertRecord :: Eq (Key record)
             => Hashable (Key record)
             => PersistEntity record
             => SqlBackend ~ PersistEntityBackend record
             => TVar (Listeners record)
             -> record
             -> AppM ()
insertRecord listeners x = do
  Env{envDatabase} <- ask
  recordKey <- flip runSqlPool envDatabase (insert x)
  liftIO $ atomically $ modifyTVar listeners $
    HashMap.insert recordKey HashMap.empty
    -- FIXME completely overwrites existing channels - memory leak? Wrong decision?

updateRecord :: Eq (Key record)
             => Hashable (Key record)
             => PersistEntity record
             => SqlBackend ~ PersistEntityBackend record
             => TVar (Listeners record)
             -> Key record
             -> record
             -> AppM ()
updateRecord listeners recordKey x = do
  Env{envDatabase} <- ask
  flip runSqlPool envDatabase (replace recordKey x)
  liftIO $ atomically $ do
    ls <- readTVar listeners
    case HashMap.lookup recordKey ls of
      Nothing -> pure ()
      Just chans -> forM_ chans $ \chan ->
        writeTChan chan (Update x)

deleteRecord :: Eq (Key record)
             => Hashable (Key record)
             => PersistEntity record
             => SqlBackend ~ PersistEntityBackend record
             => TVar (Listeners record)
             -> Key record
             -> AppM ()
deleteRecord listeners recordKey = do
  Env{envDatabase} <- ask
  flip runSqlPool envDatabase (delete recordKey)
  liftIO $ atomically $ do
    ls <- readTVar listeners
    case HashMap.lookup recordKey ls of
      Nothing -> pure ()
      Just chans -> forM_ chans $ \chan ->
        writeTChan chan Delete
    writeTVar listeners (HashMap.delete recordKey ls)


paginationServer :: forall record typ f
                  . PersistEntity record
                 => Hashable (Key record)
                 => Eq (Key record)
                 => Insertable f AppM
                 => Alternative f
                 => Foldable f
                 => SqlBackend ~ PersistEntityBackend record
                 => TVar (Listeners record)
                 -> Server AppM f
                      (PaginationInitIn (EntityField record typ))
                      (PaginationInitOut record)
                      (PaginationDeltaIn (EntityField record typ))
                      (PaginationDeltaOut record)
paginationServer listeners (PaginationInitIn pageArgs) = do
  ( argsRef :: TVar (PaginationArgs (EntityField record typ))
    ) <- liftIO (newTVarIO pageArgs)
  Env{envDatabase} <- ask
  ( threadsRef :: TVar (f (Async ()))
    ) <- liftIO (newTVarIO empty)

  let getRecords :: PaginationArgs (EntityField record typ) -> AppM ([record],[Key record])
      getRecords as = do
        ents <- flip runSqlPool envDatabase (selectList [] (paginationArgsToQuery pageArgs))
        unzip <$> forM ents (\(Persist.Entity key value) -> pure (value,key))

      bindListeners :: ServerArgs AppM (PaginationDeltaOut record) -> [Key record] -> AppM ()
      bindListeners serverArgs@ServerArgs{serverSendCurrent} ks = forM_ ks $ \key -> do
        chan <- liftIO newTChanIO
        listenerId <- liftIO (registerListener listeners key chan)
        thread <- Aligned.liftBaseWith $ \runInBase -> async $ fmap runSingleton $ runInBase $ forever $ do
          x <- liftIO $ atomically $ readTChan chan
          case x of
            Update y -> serverSendCurrent (PaginationUpdate y)
            Delete -> do
              args <- liftIO $ atomically $ readTVar argsRef
              (xs,ks') <- getRecords args
              threadsOld <- liftIO $ atomically $ do
                x <- readTVar threadsRef
                writeTVar threadsRef empty
                pure x
              bindListeners serverArgs ks'
              serverSendCurrent (PaginationFlush xs)
              liftIO (forM_ threadsOld cancel)

        threadsOld <- liftIO $ atomically $ readTVar threadsRef
        threads' <- Insert.insert thread threadsOld
        liftIO $ atomically $ writeTVar threadsRef threads'

  (xs,ks) <- getRecords pageArgs

  pure $ Just ServerContinue
    { serverContinue = \broadcast -> pure ServerReturn
      { serverInitOut = PaginationInitOut xs
      , serverOnOpen = \send -> do

        bindListeners send ks

        pure threadsRef

      , serverOnReceive = \send@ServerArgs{serverSendCurrent} x -> case x of
        PaginationChangeSize size -> do
          args <- do
            x <- liftIO $ atomically $ readTVar argsRef
            let args' = x
                  { paginationArgsPageSize = size
                  }
            liftIO $ atomically $ writeTVar argsRef args'
            pure args'

          (xs,ks) <- getRecords args
          threadsOld <- liftIO $ atomically $ do
            x <- readTVar threadsRef
            writeTVar threadsRef empty
            pure x
          bindListeners send ks
          serverSendCurrent (PaginationFlush xs)
          liftIO (forM_ threadsOld cancel)
        PaginationChangeIndex idx -> do
          args <- do
            x <- liftIO $ atomically $ readTVar argsRef
            let args' = x
                  { paginationArgsPageIndex = idx
                  }
            liftIO $ atomically $ writeTVar argsRef args'
            pure args'

          (xs,ks) <- getRecords args
          threadsOld <- liftIO $ atomically $ do
            x <- readTVar threadsRef
            writeTVar threadsRef empty
            pure x
          bindListeners send ks
          serverSendCurrent (PaginationFlush xs)
          liftIO (forM_ threadsOld cancel)
        PaginationResort field ordering -> do
          args <- do
            x <- liftIO $ atomically $ readTVar argsRef
            let args' = x
                  { paginationArgsField = field
                  , paginationArgsFieldOrdering = ordering
                  }
            liftIO $ atomically $ writeTVar argsRef args'
            pure args'

          (xs,ks) <- getRecords args
          threadsOld <- liftIO $ atomically $ do
            x <- readTVar threadsRef
            writeTVar threadsRef empty
            pure x
          bindListeners send ks
          serverSendCurrent (PaginationFlush xs)
          liftIO (forM_ threadsOld cancel)
      }
    , serverOnUnsubscribe = do
      threadsOld <- liftIO $ atomically $ do
        x <- readTVar threadsRef
        writeTVar threadsRef empty
        pure x
      liftIO (forM_ threadsOld cancel)
    }
