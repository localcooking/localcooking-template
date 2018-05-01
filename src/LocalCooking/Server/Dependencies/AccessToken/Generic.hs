{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleContexts
  , RecordWildCards
  , NamedFieldPuns
  , RankNTypes
  , FunctionalDependencies
  , ScopedTypeVariables
  #-}

module LocalCooking.Server.Dependencies.AccessToken.Generic where

import LocalCooking.Common.AccessToken (AccessToken, genAccessToken)
import Web.Dependencies.Sparrow.Types (Server, ServerContinue (..), ServerReturn (..), ServerArgs (..))

import Data.Hashable (Hashable)
import Data.Time (NominalDiffTime)
import Data.TimeMap (TimeMap, newTimeMap)
import qualified Data.TimeMap as TimeMap
import Data.Singleton.Class (Extractable (runSingleton))
import Control.Monad (forM_, forever)
import qualified Control.Monad.Trans.Control.Aligned as Aligned
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TMapMVar.Hash (TMapMVar, newTMapMVar)
import qualified Control.Concurrent.STM.TMapMVar.Hash as TMapMVar
import Control.Newtype (Newtype (pack, unpack))


class AccessTokenInitIn initIn where
  getExists :: initIn -> Maybe AccessToken


class AccessTokenInitOut initOut err | initOut -> err where
  makeSuccess :: AccessToken -> initOut
  makeFailure :: err -> initOut


class AccessTokenDeltaOut deltaOut where
  makeRevoke :: deltaOut



data AccessTokenContext k a = AccessTokenContext
  { accessTokenContextSubject :: TimeMap k a
  , accessTokenContextExpire  :: TMapMVar k () -- decay lock
  }



newAccessTokenContext :: Newtype k AccessToken => STM (AccessTokenContext k a)
newAccessTokenContext = AccessTokenContext <$> newTimeMap <*> newTMapMVar



revokeAccess :: Hashable k
             => Eq k
             => AccessTokenContext k a
             -> k
             -> STM ()
revokeAccess AccessTokenContext{..} accessToken = do
  TimeMap.delete accessToken accessTokenContextSubject
  TMapMVar.insert accessTokenContextExpire accessToken ()


insertAccess :: Hashable k
             => Eq k
             => Newtype k AccessToken
             => AccessTokenContext k a
             -> a
             -> IO k
insertAccess AccessTokenContext{accessTokenContextSubject} x = do
  accessToken <- pack <$> genAccessToken
  TimeMap.insert accessToken x accessTokenContextSubject
  pure accessToken



lookupAccess :: Hashable k
             => Eq k
             => AccessTokenContext k a
             -> k
             -> IO (Maybe a)
lookupAccess AccessTokenContext{accessTokenContextSubject} accessToken = do
  mSubj <- atomically (TimeMap.lookup accessToken accessTokenContextSubject)
  TimeMap.touch accessToken accessTokenContextSubject
  pure mSubj



expireThread  :: Hashable k
              => Eq k
              => NominalDiffTime
              -> AccessTokenContext k a
              -> IO ()
expireThread expiration AccessTokenContext{..} = forever $ do
  xs <- TimeMap.takeFromNow expiration accessTokenContextSubject
  forM_ xs $ \(authToken,_) ->
    atomically (TMapMVar.insert accessTokenContextExpire authToken ())
  threadDelay $
    let second = 10 ^ 6
        minute = second * 60
    in  minute



accessTokenServer :: forall k a initIn initOut err deltaIn deltaOut m stM
                   . Newtype k AccessToken
                  => Hashable k
                  => Eq k
                  => AccessTokenInitIn initIn
                  => AccessTokenInitOut initOut err
                  => AccessTokenDeltaOut deltaOut
                  => MonadIO m
                  => Aligned.MonadBaseControl IO m stM
                  => Extractable stM
                  => AccessTokenContext k a
                  -> (initIn -> m (Either (Maybe err) k)) -- ^ obtain init auth token
                  -> (m () -> deltaIn -> m ()) -- ^ revocation reactions
                  -> (m () -> m ()) -- ^ async revocations
                  -> Server m initIn initOut deltaIn deltaOut
accessTokenServer
  context@AccessTokenContext{accessTokenContextExpire}
  getAccessToken
  revokeOnDeltaIn
  revokeOnOpen
  = \initIn -> do
  let serverReturnSuccess :: k -> ServerContinue m initOut deltaIn deltaOut
      serverReturnSuccess accessToken =
        let revokeAccess' serverReject = do
              liftIO $ atomically $ revokeAccess context accessToken
              serverReject
        in  ServerContinue
            { serverOnUnsubscribe = pure ()
            , serverContinue = \_ -> pure ServerReturn
              { serverInitOut = makeSuccess (unpack accessToken)
              , serverOnOpen = \ServerArgs{serverSendCurrent,serverDeltaReject} -> do
                  revokeThread <- Aligned.liftBaseWith $ \runInBase -> async $ do
                    () <- atomically (TMapMVar.lookup accessTokenContextExpire accessToken)
                    runSingleton <$> runInBase (serverSendCurrent makeRevoke)
                  onOpenThread <- Aligned.liftBaseWith $ \runInBase -> async $
                    runSingleton <$> runInBase (revokeOnOpen (revokeAccess' serverDeltaReject))
                  pure [revokeThread,onOpenThread]
              , serverOnReceive = \ServerArgs{serverDeltaReject} deltaIn ->
                  revokeOnDeltaIn (revokeAccess' serverDeltaReject) deltaIn
              }
            }

  case getExists initIn of
    Just accessToken -> do
      mSubj <- liftIO (lookupAccess context (pack accessToken))
      case mSubj of
        Nothing -> pure Nothing
        Just _ -> pure $ Just $ serverReturnSuccess (pack accessToken)
    Nothing -> do
      mAccess <- getAccessToken initIn
      case mAccess of
        Right accessToken -> pure $ Just $ serverReturnSuccess accessToken
        Left mErr -> case mErr of
          Nothing -> pure Nothing
          Just err -> pure $ Just ServerContinue
            { serverOnUnsubscribe = pure ()
            , serverContinue = \_ -> pure ServerReturn
              { serverInitOut = makeFailure err
              , serverOnOpen = \ServerArgs{serverDeltaReject} -> do
                  serverDeltaReject
                  pure []
              , serverOnReceive = \_ _ -> pure ()
              }
            }

