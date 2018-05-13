{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleContexts
  , RecordWildCards
  , NamedFieldPuns
  , RankNTypes
  , FunctionalDependencies
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

{-|

Module: LocalCooking.Server.Dependencies.AccessToken.Generic
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Server.Dependencies.AccessToken.Generic where

import LocalCooking.Common.AccessToken (AccessToken, genAccessToken)
import Web.Dependencies.Sparrow.Types (Server, ServerContinue (..), ServerReturn (..), ServerArgs (..))

import Data.Hashable (Hashable)
import Data.Time (NominalDiffTime)
import Data.TimeMap (TimeMap, newTimeMap)
import qualified Data.TimeMap as TimeMap
import Data.Singleton.Class (Extractable (runSingleton))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object, Value (String, Object))
import Data.Aeson.Types (typeMismatch)
import Data.Insert.Class (Insertable (insert))
import Control.Applicative (Alternative (empty))
import Control.Monad (forM_, forever)
import qualified Control.Monad.Trans.Control.Aligned as Aligned
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (STM, atomically, newTVarIO)
import Control.Concurrent.STM.TMapMVar.Hash (TMapMVar, newTMapMVar)
import qualified Control.Concurrent.STM.TMapMVar.Hash as TMapMVar
import Control.Newtype (Newtype (pack, unpack))


-- * Classes

class AccessTokenInitIn initIn where
  getExists :: initIn -> Maybe AccessToken


class AccessTokenInitOut initOut err | initOut -> err where
  makeSuccess :: AccessToken -> initOut
  makeFailure :: err -> initOut


class AccessTokenDeltaOut deltaOut where
  makeRevoke :: deltaOut


-- * Context

-- | Global context for an access token - expiring mapping with concurrent locks
data AccessTokenContext k a = AccessTokenContext
  { accessTokenContextSubject :: TimeMap k a
  , accessTokenContextExpire  :: TMapMVar k () -- ^ Expiration lock
  }



newAccessTokenContext :: Newtype k AccessToken => STM (AccessTokenContext k a)
newAccessTokenContext = AccessTokenContext <$> newTimeMap <*> newTMapMVar


-- * Utilities

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



-- * Global Functions

-- | Independently forked thread that drains the timemap and invokes the concurrency locks when
-- tokens expire.
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


-- | Create a Sparrow dependency for the access token implementation
accessTokenServer :: forall k a initIn initOut err deltaIn deltaOut f m stM
                   . Newtype k AccessToken
                  => Hashable k
                  => Eq k
                  => Insertable f m
                  => Alternative f
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
                  -> Server m f initIn initOut deltaIn deltaOut
accessTokenServer
  context@AccessTokenContext{accessTokenContextExpire}
  getAccessToken
  revokeOnDeltaIn
  revokeOnOpen
  = \initIn -> do
  let serverReturnSuccess :: k -> ServerContinue m f initOut deltaIn deltaOut
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
                  threads <- insert onOpenThread empty >>= insert revokeThread
                  threadsRef <- liftIO (newTVarIO threads)
                  pure threadsRef
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
                  threadsRef <- liftIO (newTVarIO empty)
                  pure threadsRef
              , serverOnReceive = \_ _ -> pure ()
              }
            }



-- * Generic Utility Types

-- | Authenticated initIn messages
data AuthInitIn k a = AuthInitIn
  { authInitInToken   :: k
  , authInitInSubject :: a
  }

instance (FromJSON k, FromJSON a) => FromJSON (AuthInitIn k a) where
  parseJSON json = case json of
    Object o -> AuthInitIn <$> o .: "token" <*> o .: "subj"
    _ -> fail
    where
      fail = typeMismatch "AuthInitIn" json


-- | Authenticated responses, with failure
data AuthInitOut a
  = AuthInitOutNoAuth
  | AuthInitOut
    { authInitOut :: a
    }

instance ToJSON a => ToJSON (AuthInitOut a) where
  toJSON x = case x of
    AuthInitOutNoAuth -> String "no-auth"
    AuthInitOut x -> object ["subj" .= x]
