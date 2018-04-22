{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , QuasiQuotes
  #-}

module LocalCooking.Server.HTTP where

import LocalCooking.Server.Assets (privacyPolicy)
import LocalCooking.Server.Dependencies.AuthToken (authTokenServer, AuthTokenInitIn (AuthTokenInitInFacebookCode), AuthTokenInitOut (AuthTokenInitOutSuccess))
import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..), Development (..))
import LocalCooking.Template (html)
import LocalCooking.Links.Class (LocalCookingSiteLinks (rootLink, registerLink))
import LocalCooking.Auth.Error (AuthError (..), PreliminaryAuthToken (..))
import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Colors (LocalCookingColors)
import Facebook.Types (FacebookLoginCode (..))
import Facebook.State (FacebookLoginState (..))

import Web.Routes.Nested (RouterT, match, matchHere, matchAny, action, post, get, text, textOnly, l_, (</>), o_, route)
import Web.Dependencies.Sparrow.Types (ServerContinue (ServerContinue, serverContinue), ServerReturn (ServerReturn, serverInitOut))
import Network.Wai (strictRequestBody, queryString)
import Network.Wai.Middleware.ContentType (bytestring, FileExt (Other, JavaScript, Html))
import Network.Wai.Trans (MiddlewareT)
import Network.HTTP.Types (status302)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as BS16
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS8
import Data.URI (URI (..), printURI)
import Data.Url (packLocation)
import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy (..))
import Data.Monoid ((<>))
import qualified Data.Strict.Maybe as Strict
import Path.Extended ((<&>), ToLocation (..), FromLocation)
import Text.Heredoc (here)
import Control.Applicative ((<|>))
import Control.Monad (join, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Control.Logging (log')
import qualified Crypto.Saltine.Class as NaCl




router :: forall siteLinks sec
        . LocalCookingSiteLinks siteLinks
       => FromLocation siteLinks
       => ToLocation siteLinks
       => BS.ByteString -- ^ Unminified
       -> BS.ByteString -- ^ Minified
       -> [(FilePath, BS.ByteString)] -- ^ Favicons
       -> LocalCookingColors
       -> Proxy siteLinks
       -> ((siteLinks -> MiddlewareT AppM) -> RouterT (MiddlewareT AppM) sec AppM ())
       -> RouterT (MiddlewareT AppM) sec AppM ()
router
  frontend
  frontendMin
  favicons
  colors
  Proxy
  handles
  = do
  Env{envHostname,envTls} <- lift ask

  let handleAuthToken :: siteLinks
                      -> MiddlewareT AppM
      handleAuthToken link app req resp =
        case join $ lookup "authToken" $ queryString req of
          Nothing ->
            (action $ get $ html colors Nothing link "") app req resp
          Just json -> case Aeson.decode $ LBS.fromStrict json of
            Nothing ->
              (action $ get $ html colors Nothing link "") app req resp
            Just (PreliminaryAuthToken mEToken) ->
              (action $ get $ html colors mEToken link "") app req resp

  -- main routes
  matchHere (handleAuthToken rootLink)
  match (l_ "about" </> o_) (handleAuthToken rootLink)
  match (l_ "register" </> o_) (handleAuthToken registerLink)
  handles handleAuthToken
  matchAny $ \_ _ resp -> do
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

  -- privacy policy
  match (l_ "privacypolicy" </> o_) $ action $ get $
    bytestring Html (LBS.fromStrict privacyPolicy)

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
  match (l_ "facebookLoginReturn" </> o_) $ \_ req resp -> do
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
           -> LocalCookingColors
           -> Proxy siteLinks
           -> ((siteLinks -> MiddlewareT AppM) -> RouterT (MiddlewareT AppM) sec AppM ()) -- ^ HTTP handlers
           -> RouterT (MiddlewareT AppM) sec AppM () -- ^ Dependencies
           -> MiddlewareT AppM
httpServer frontend frontendEnv favicons colors siteLinks handlers dependencies =
  route $ do
    dependencies
    router frontend frontendEnv favicons colors siteLinks handlers
