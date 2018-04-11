{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , ScopedTypeVariables
  , RecordWildCards
  , DataKinds
  , QuasiQuotes
  #-}

module LocalCooking.Server.HTTP where

import LocalCooking.Server.Dependencies.AuthToken (authTokenServer, AuthTokenInitIn (AuthTokenInitInFacebookCode), AuthTokenInitOut (AuthTokenInitOutSuccess))
import LocalCooking.Types (AppM, runAppM, HTTPException (..))
import LocalCooking.Types.Env (Env (..), Managers (..), isDevelopment, Development (..))
import LocalCooking.Types.FrontendEnv (FrontendEnv (..))
import LocalCooking.Types.Keys (Keys (..))
import LocalCooking.Template (html)
import LocalCooking.Links.Class (LocalCookingSiteLinks (rootLink))
import LocalCooking.Auth.Error (AuthError (..), PreliminaryAuthToken (..))
import LocalCooking.Common.AuthToken (AuthToken)
import Facebook.Types (FacebookLoginCode (..))
import Facebook.State (FacebookLoginState (..))

import Web.Routes.Nested (RouterT, match, matchHere, matchGroup, matchAny, action, post, get, json, text, textOnly, l_, (</>), o_, route)
import Web.Dependencies.Sparrow.Types (ServerContinue (ServerContinue, serverContinue), ServerReturn (ServerReturn, serverInitOut))
import Network.Wai (strictRequestBody, queryString)
import Network.Wai.Middleware.ContentType (bytestring, FileExt (Other, JavaScript))
import Network.Wai.Trans (MiddlewareT)
import Network.WebSockets (defaultConnectionOptions)
import Network.WebSockets.Trans (websocketsOrT)
import Network.HTTP.Types (status302)
import Network.HTTP.Client (httpLbs, responseBody, parseRequest)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base64    as BS64
import qualified Data.ByteString.Base16    as BS16
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS8
import Data.URI (URI (..), printURI)
import Data.Url (packLocation)
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson.Types (typeMismatch, Value (String, Object))
import qualified Data.Aeson as Aeson
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TimeMap
import Data.TimeMap.Multi (TimeMultiMap)
import qualified Data.TimeMap.Multi as TimeMultiMap
import qualified Data.Attoparsec.Text as Atto
import Data.Proxy (Proxy (..))
import Data.Monoid ((<>))
import qualified Data.Strict.Maybe as Strict
import Data.Strict.Tuple (Pair (..))
import Path.Extended ((<&>), ToLocation (..), FromLocation)
import Text.Heredoc (here)
import Control.Applicative ((<|>))
import Control.Monad (join, when, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Control.Exception.Safe (throwM)
import Control.Logging (log', warn')
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMapChan (TMapChan, newTMapChan)
import qualified Control.Concurrent.STM.TMapChan as TMapChan
import Control.Concurrent.Chan.Scope (Scope (..))
import Control.Concurrent.STM.TChan.Typed (TChanRW)
import Crypto.Saltine.Core.Box (newNonce)
import qualified Crypto.Saltine.Class as NaCl
import System.IO.Error (userError)


handleAuthToken :: MiddlewareT AppM
handleAuthToken app req resp =
  case join $ lookup "authToken" $ queryString req of
    Nothing -> do
      (action $ get $ html Nothing "") app req resp
    Just json -> case Aeson.decode $ LBS.fromStrict json of
      Nothing -> do
        (action $ get $ html Nothing "") app req resp
      Just x@(PreliminaryAuthToken mEToken) -> do
        (action $ get $ html mEToken "") app req resp



router :: forall siteLinks sec
        . LocalCookingSiteLinks siteLinks
       => FromLocation siteLinks
       => ToLocation siteLinks
       => BS.ByteString -- ^ Unminified
       -> BS.ByteString -- ^ Minified
       -> [(FilePath, BS.ByteString)] -- ^ Favicons
       -> Proxy siteLinks
       -> (MiddlewareT AppM -> RouterT (MiddlewareT AppM) sec AppM ())
       -> RouterT (MiddlewareT AppM) sec AppM ()
router
  frontend
  frontendMin
  favicons
  Proxy
  handles
  = do
  Env{envHostname,envTls} <- lift ask

  -- main routes
  matchHere handleAuthToken
  match (l_ "about" </> o_) handleAuthToken
  match (l_ "register" </> o_) handleAuthToken
  handles handleAuthToken
  matchAny $ \app req resp -> do
    let redirectUri = URI (Strict.Just $ if envTls then "https" else "http")
                          True
                          envHostname
                          []
                          []
                          Strict.Nothing
    resp $ textOnly "" status302 [("Location", T.encodeUtf8 $ printURI redirectUri)]

  match (l_ "robots" </> o_) $ action $ get $ text [here|User-agent: *
Disallow: /dependencies/
Disallow: /facebookLoginReturn
Disallow: /facebookLoginDeauthorize
|]

  -- favicons
  forM_ favicons $ \(file, content) -> do
    let (file', ext) = T.breakOn "." (T.pack file)
    match (l_ file' </> o_) $ action $ get $
      bytestring (Other (T.dropWhile (== '.') ext)) (LBS.fromStrict content)

  -- application
  match (l_ "index" </> o_) $ \app req resp -> do
    Env{envDevelopment} <- ask
    let mid = case envDevelopment of
          Nothing -> action $ get $ bytestring JavaScript $ LBS.fromStrict frontendMin
          Just Development{devCacheBuster} -> case join $ lookup "cache_buster" $ queryString req of
            Nothing -> fail "No cache busting parameter!"
            Just cacheBuster
              | cacheBuster == BS16.encode (NaCl.encode devCacheBuster) ->
                  action $ get $ bytestring JavaScript $ LBS.fromStrict frontend
              | otherwise -> fail "Wrong cache buster!" -- FIXME make cache buster generic
    mid app req resp

  -- TODO handle authenticated linking
  match (l_ "facebookLoginReturn" </> o_) $ \app req resp -> do
    let qs = queryString req
    ( eToken :: Either AuthError AuthToken
      , mFbState :: Maybe (FacebookLoginState siteLinks)
      ) <- case do  let bad = do
                          errorCode <- join $ lookup "error_code" qs
                          errorMessage <- join $ lookup "error_message" qs
                          pure $ Left $ FBLoginReturnBad (T.decodeUtf8 errorCode) (T.decodeUtf8 errorMessage)
                        denied = do
                          error' <- join $ lookup "error" qs
                          errorReason <- join $ lookup "error_reason" qs
                          errorDescription <- join $ lookup "error_description" qs
                          if error' == "access_denied" && errorReason == "user_denied"
                            then pure $ Left $ FBLoginReturnDenied $ T.decodeUtf8 errorDescription
                            else Nothing
                        good = do
                          code <- fmap T.decodeUtf8 $ join $ lookup "code" qs
                          (state :: FacebookLoginState siteLinks) <- do
                            x <- join $ lookup "state" qs
                            join $ Aeson.decode $ LBS.fromStrict x
                          pure $ Right (FacebookLoginCode code, state)
                    bad <|> good <|> denied of

              Nothing -> do
                liftIO $ do
                  putStr "Bad /facebookLoginReturn parse: "
                  print qs
                pure (Left FBLoginReturnBadParse, Nothing)
              Just eX -> case eX of
                Left e -> pure (Left e, Nothing)
                Right (code, state) -> do
                  mCont <- authTokenServer $ AuthTokenInitInFacebookCode code
                  case mCont of
                    Nothing -> pure (Left FBLoginReturnNoUser, Just state)
                    Just ServerContinue{serverContinue} -> do
                      ServerReturn{serverInitOut = AuthTokenInitOutSuccess authToken} <- serverContinue undefined
                      pure (Right authToken, Just state)

    let redirectUri =
          packLocation
            (Strict.Just $ if envTls then "https" else "http")
            True
            envHostname $
              let loc = toLocation $
                          case mFbState of
                            Nothing -> rootLink
                            Just FacebookLoginState{facebookLoginStateOrigin} ->
                              facebookLoginStateOrigin
              in  loc <&> ( "authToken"
                          , Just $ LBS8.toString $ Aeson.encode $ PreliminaryAuthToken $ Just eToken
                          )
    resp $ textOnly "" status302 [("Location", T.encodeUtf8 $ printURI redirectUri)]

  -- TODO only allow facebook's ip as sockHost client?
  match (l_ "facebookLoginDeauthorize" </> o_) $ \app req resp -> do
    body <- liftIO $ strictRequestBody req
    log' $ "Got deauthorized: " <> T.pack (show body)
    (action $ post $ \_ -> text "") app req resp




httpServer :: LocalCookingSiteLinks siteLinks
           => FromLocation siteLinks
           => ToLocation siteLinks
           => BS.ByteString -- ^ Unminified
           -> BS.ByteString -- ^ Minified
           -> [(FilePath, BS.ByteString)] -- ^ Favicons
           -> Proxy siteLinks
           -> (MiddlewareT AppM -> RouterT (MiddlewareT AppM) sec AppM ()) -- ^ HTTP handlers
           -> RouterT (MiddlewareT AppM) sec AppM () -- ^ Dependencies
           -> MiddlewareT AppM
httpServer frontend frontendEnv favicons siteLinks handlers dependencies = \app req resp -> do
  route ( do dependencies
             router frontend frontendEnv favicons siteLinks handlers
        ) app req resp
