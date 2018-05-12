{-# LANGUAGE
    RecordWildCards
  , NamedFieldPuns
  , GADTs
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Server.Dependencies.Pagination where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (forM_)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Concurrent.STM (STM, atomically, TVar, readTVar, writeTVar, modifyTVar, TChan, writeTChan)
import Database.Persist.Sql (runSqlPool, SqlBackend)
import Database.Persist.Class (PersistEntity (Key, EntityField, PersistEntityBackend), selectList, insert, replace, delete)
import qualified Database.Persist.Types as Persist
import Web.Dependencies.Sparrow (Server)


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

insertListener :: Eq (Key record)
               => Hashable (Key record)
               => TVar (Listeners record)
               -> Key record
               -> TChan (ListenerMsg record)
               -> IO ListenerId
insertListener listeners recordKey chan = do
  key <- ListenerId <$> nextRandom
  atomically $ modifyTVar listeners $
    HashMap.alter
      (maybe (Just (HashMap.singleton key chan)) (Just . HashMap.insert key chan))
      recordKey
  pure key

deleteListener :: Eq (Key record)
               => Hashable (Key record)
               => TVar (Listeners record)
               -> Key record
               -> ListenerId
               -> STM ()
deleteListener listeners recordKey key =
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


paginationServer :: PersistEntity record
                 => SqlBackend ~ PersistEntityBackend record
                 => TVar (Listeners record)
                 -> Server AppM
                      (PaginationInitIn (EntityField record typ))
                      (PaginationInitOut record)
                      (PaginationDeltaIn (EntityField record typ))
                      (PaginationDeltaOut record)
paginationServer listeners (PaginationInitIn pageArgs) = do
  Env{envDatabase} <- ask
  ents <- flip runSqlPool envDatabase (selectList [] (paginationArgsToQuery pageArgs))
  forM_ ents $ \(Persist.Entity key value) ->
    undefined
  undefined
