{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , ScopedTypeVariables
  , DataKinds
  , QuasiQuotes
  , RecordWildCards
  #-}

{-|

Module: LocalCooking.Server.HTTP
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Server.HTTP where

import LocalCooking.Server.Assets (privacyPolicy)
import LocalCooking.Server.Dependencies.AuthToken (authTokenServer, AuthTokenInitIn (AuthTokenInitInFacebookCode), AuthTokenInitOut (..), AuthTokenFailure (..), PreliminaryAuthToken (..))
import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..), Development (..))
import LocalCooking.Template (html)
import LocalCooking.Links.Class (LocalCookingSiteLinks (rootLink, registerLink))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Colors (LocalCookingColors)
import Facebook.Types (FacebookLoginCode (..))
import Facebook.State (FacebookLoginState (..), FacebookLoginUnsavedFormData (..))

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
import Data.Maybe (fromMaybe)
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



-- | Majority of server-related logic.
router :: forall siteLinks sec
        . LocalCookingSiteLinks siteLinks
       => FromLocation siteLinks
       => ToLocation siteLinks
       => BS.ByteString -- ^ Unminified
       -> BS.ByteString -- ^ Minified
       -> [(FilePath, BS.ByteString)] -- ^ Favicons
       -> LocalCookingColors
       -> Proxy siteLinks
       -> ((siteLinks -> MiddlewareT AppM) -> RouterT (MiddlewareT AppM) sec AppM ()) -- ^ Casual HTTP endpoints
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

  -- Turns a handled route's potential `?authToken=<preliminary>` into a FrontendEnv
  let handleAuthToken :: siteLinks
                      -> MiddlewareT AppM
      handleAuthToken link app req resp =
        let preliminary = case join $ lookup "authToken" $ queryString req of
              Nothing -> PreliminaryAuthToken Nothing
              Just json -> fromMaybe (PreliminaryAuthToken Nothing) (Aeson.decode (LBS.fromStrict json))
            formData = case join $ lookup "formData" $ queryString req of
              Nothing -> Nothing
              Just json -> Aeson.decode (LBS.fromStrict json)
        in  (action $ get $ html colors Nothing preliminary formData link "") app req resp

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

  match (l_ "emailConfirm" </> o_) $ \app req resp -> do
    let redirectUri = URI (Strict.Just $ if envTls then "https" else "http")
                          True
                          envHostname
                          []
                          []
                          Strict.Nothing
        def = resp $ textOnly "" status302 [("Location", T.encodeUtf8 $ printURI redirectUri)]
    case join $ lookup "emailToken" $ queryString req of
      Nothing -> def
      Just json -> case Aeson.decode (LBS.fromStrict json) of
        Nothing -> def
        Just emailToken ->
          (action $ get $ html colors
            (Just emailToken)
            (PreliminaryAuthToken Nothing)
            Nothing (rootLink :: siteLinks) "") app req resp

  -- TODO handle authenticated linking
  match (l_ "facebookLoginReturn" </> o_) $ \_ req resp -> do
    let qs = queryString req
    ( eToken :: Either AuthTokenFailure AuthToken
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

              Nothing -> pure (Left FBLoginReturnBadParse, Nothing)
              Just eX -> case eX of
                Left e -> pure (Left e, Nothing)
                -- Successfully fetched the fbCode and FacebookState from query string
                Right (code, state) -> do
                  -- Manually invoke the AuthToken dependency's AuthTokenInitIn as Haskell code
                  mCont <- authTokenServer (AuthTokenInitInFacebookCode code)
                  case mCont of
                    Nothing -> pure (Left FBLoginReturnBadParse, Just state)
                    Just ServerContinue{serverContinue} -> do
                      -- FIXME partial, could fail feasibly
                      ServerReturn{serverInitOut} <- serverContinue undefined
                      case serverInitOut of
                        AuthTokenInitOutSuccess authToken ->
                          pure (Right authToken, Just state)
                        AuthTokenInitOutFailure e ->
                          pure (Left e, Just state)

    let redirectUri :: URI
        redirectUri =
          packLocation
            (Strict.Just $ if envTls then "https" else "http")
            True
            envHostname $ case mFbState of
              -- Facebook rejected it
              Nothing ->
                let loc = toLocation (rootLink :: siteLinks)
                in  loc <&> ( "authToken"
                            , Just $ LBS8.toString $ Aeson.encode $ PreliminaryAuthToken $ Just eToken
                            )
              Just FacebookLoginState{..} ->
                case eToken of
                  -- Redirect to register page, populate with fbUserId form data
                  Left (FBLoginReturnNoUser fbUserId) ->
                    let loc = toLocation (registerLink :: siteLinks)
                    in  loc <&> ( "authToken"
                                , Just $ LBS8.toString $ Aeson.encode $ PreliminaryAuthToken $ Just eToken
                                )
                            <&> ( "formData"
                                , Just $ LBS8.toString $ Aeson.encode $ case facebookLoginStateFormData of
                                  Nothing -> FacebookLoginUnsavedFormDataRegister
                                    { facebookLoginUnsavedFormDataRegisterEmail = ""
                                    , facebookLoginUnsavedFormDataRegisterEmailConfirm = ""
                                    , facebookLoginUnsavedFormDataRegisterFBUserId = Just fbUserId
                                    }
                                  Just formData -> formData
                                    { facebookLoginUnsavedFormDataRegisterFBUserId = Just fbUserId
                                    -- FIXME pack fbUserId as unsaved form data
                                    }
                                )
                  _ ->
                    let loc = toLocation facebookLoginStateOrigin
                        -- Add `?authToken=<preliminary>` query param to redirect origin
                        loc' = loc <&> ( "authToken"
                                       , Just $ LBS8.toString $ Aeson.encode $ PreliminaryAuthToken $ Just eToken
                                       )
                    in  case facebookLoginStateFormData of
                          Just formData ->
                            loc' <&> ( "formData"
                                     , Just $ LBS8.toString $ Aeson.encode formData
                                     )
                          Nothing -> loc'

    resp $ textOnly "" status302 [("Location", T.encodeUtf8 $ printURI redirectUri)]

  -- TODO only allow facebook's ip as sockHost client?
  match (l_ "facebookLoginDeauthorize" </> o_) $ \app req resp -> do
    body <- liftIO $ strictRequestBody req
    log' $ "Got deauthorized: " <> T.pack (show body)
    (action $ post $ \_ -> text "") app req resp



-- | HTTP server, post-routing
httpServer :: LocalCookingSiteLinks siteLinks
           => FromLocation siteLinks
           => ToLocation siteLinks
           => BS.ByteString -- ^ Unminified
           -> BS.ByteString -- ^ Minified
           -> [(FilePath, BS.ByteString)] -- ^ Favicons
           -> LocalCookingColors
           -> Proxy siteLinks
           -> ((siteLinks -> MiddlewareT AppM) -> RouterT (MiddlewareT AppM) sec AppM ()) -- ^ HTTP handlers
           -> RouterT (MiddlewareT AppM) sec AppM () -- ^ Dependencies, post sparrow serving
           -> MiddlewareT AppM
httpServer frontend frontendEnv favicons colors siteLinks handlers dependencies =
  route $ do
    dependencies
    router frontend frontendEnv favicons colors siteLinks handlers
