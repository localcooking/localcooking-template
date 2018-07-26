{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , NamedFieldPuns
  , QuasiQuotes
  , StandaloneDeriving
  , MultiParamTypeClasses
  , RecordWildCards
  #-}

{-|

Module: Google.Keys
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

HTML Rendering tools

-}

module LocalCooking.Template where

import           LocalCooking.Types (isDevelopment, Env (..))
import           LocalCooking.Function.System (SystemM, Keys (..), SystemEnv (..), getSystemEnv)
-- import           LocalCooking.Types (SystemM)
-- import           LocalCooking.Types.Env (Env (..), Development (..), isDevelopment)
import           LocalCooking.Types.ServerToClient (ServerToClient (..))
-- import           LocalCooking.Types.Keys (Keys (..))
import           LocalCooking.Semantics.Common (ConfirmEmailError)
import           LocalCooking.Dependencies.AuthToken (PreliminaryAuthToken (..))
import           LocalCooking.Colors (LocalCookingColors (..))
import           LocalCooking.Links.Class (LocalCookingSiteLinks (toDocumentTitle))
import           LocalCooking.Common.AccessToken.Email (EmailToken)
import           Facebook.Types (FacebookAppCredentials (..))
import           Facebook.State (FacebookLoginUnsavedFormData)
import           Google.Keys (GoogleCredentials (..), googleAnalyticsGTagToURI, GoogleAnalyticsGTag (..), googleReCaptchaAssetURI)

import           Lucid (renderBST, HtmlT, Attribute, content_, name_, meta_, httpEquiv_, charset_, link_, rel_, type_, href_, sizes_, script_, src_, async_)
import           Lucid.Base (makeAttribute)
import           Network.HTTP.Types (Status, status200)
import qualified Network.Wai.Middleware.ContentType.Types as CT
import           Web.Page.Lucid (template, WebPage (..))
import           Web.Routes.Nested (FileExtListenerT, mapHeaders, mapStatus, bytestring)

import qualified Data.Text                                as T
import           Data.Default
import qualified Data.HashMap.Strict                      as HM
import           Data.Markup                              as M
import           Data.Url (AbsoluteUrlT (..), packLocation)
import           Data.URI (URI (..), printURI)
import           Data.URI.Auth (URIAuth (..))
import           Data.URI.Auth.Host (URIAuthHost (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16                   as BS16
import qualified Data.ByteString.UTF8                     as BS8
import qualified Data.Strict.Maybe                        as Strict
import           Control.Monad.Trans                      (lift)
import           Control.Monad.Reader                     (ask)
import           Control.Monad.State                      (modify)
import           Path (Abs, File, absfile)
import           Path.Extended (ToPath (..), ToLocation (..), fromAbsFile, addQuery)
import           Text.Julius (julius, renderJavascriptUrl)
import           Text.Lucius (lucius, renderCssUrl)
import           Crypto.Saltine.Core.Box (Nonce)
import qualified Crypto.Saltine.Class                     as NaCl


deriving instance Show URIAuthHost
deriving instance Show URIAuth
deriving instance Show URI


-- | For raw HTML responses
htmlLight :: Env
          -> Status
          -> HtmlT (AbsoluteUrlT SystemM) a
          -> FileExtListenerT SystemM ()
htmlLight Env{envMkURI} s content = do
  htmlBS <- lift $ do
    -- Env{envMkURI} <- getEnv
    -- Env{envHostname,envTls} <- ask
    -- let locationToURI = packLocation (Strict.Just $ if envTls then "https" else "http") True envHostname
    runAbsoluteUrlT (renderBST content) envMkURI -- locationToURI

  bytestring CT.Html htmlBS
  modify . HM.map $ mapStatus (const s)
                  . mapHeaders ([("content-Type", "text/html")] ++)


-- | Wrap some HTML into a 'masterPage' template, and issue it as a @text/html@ response.
html :: LocalCookingSiteLinks siteLinks
     => Env
     -> LocalCookingColors
     -> Maybe ConfirmEmailError
     -> Maybe PreliminaryAuthToken
     -> Maybe FacebookLoginUnsavedFormData
     -> siteLinks
     -> HtmlT (AbsoluteUrlT SystemM) ()
     -> FileExtListenerT SystemM ()
html env colors mConfEmail preliminary formData link =
  htmlLight env status200 . mainTemplate env colors mConfEmail preliminary formData link


-- TODO Consolidate instance arguments into datum

-- | Top-level scaffolding for the site
masterPage :: LocalCookingSiteLinks siteLinks
           => Env
           -> LocalCookingColors -- ^ Site colors
           -> Maybe ConfirmEmailError -- ^ Fetched from @?emailToken=...@ query parameter - email confirmation
           -> Maybe PreliminaryAuthToken -- ^ Fetched from @?authToken=...@ query parameter
           -> Maybe FacebookLoginUnsavedFormData -- ^ Fetched from @?formData=...@ query parameter
           -> siteLinks -- ^ Site link being represented
           -> WebPage (HtmlT (AbsoluteUrlT SystemM) ()) T.Text [Attribute]
masterPage env LocalCookingColors{..} mConfEmail preliminary formData link =
  let page :: WebPage (HtmlT (AbsoluteUrlT SystemM) ()) T.Text [Attribute]
      page = def
  in  page
        { metaVars = do
            link_ [href_ "https://fonts.googleapis.com/css?family=Roboto:300,400,500", rel_ "stylesheet"]
            link_ [href_ "https://cdnjs.cloudflare.com/ajax/libs/flag-icon-css/2.8.0/css/flag-icon.min.css", rel_ "stylesheet"]
            meta_ [charset_ "utf-8"]
            meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge,chrome=1"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, maximum-scale=1.0"]
            link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/apple-touch-icon.png"]
            link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/favicon-32x32.png"]
            link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/favicon-16x16.png"]
            link_ [rel_ "manifest", href_ "/site.webmanifest"]
            link_ [rel_ "mask-icon", href_ "/safari-pinned-tab.svg", makeAttribute "color" "#c62828"]
            meta_ [name_ "msapplication-TileColor", content_ "#c62828"]
            meta_ [name_ "theme-color", content_ "#ffffff"]
        , pageTitle = toDocumentTitle link
        , styles =
          deploy M.Css Inline $ renderCssUrl (\_ _ -> undefined) inlineStyles
        , bodyScripts = do
          -- Env{envDevelopment = mDev} <- lift ask -- TODO dev env
          deploy M.JavaScript M.Remote $ toLocation $ IndexJs Nothing -- $ devCacheBuster <$> mDev
        , afterStylesScripts = do
          SystemEnv
            { systemEnvKeys = Keys
              { keysFacebook = FacebookAppCredentials{clientId}
              , keysGoogle = GoogleCredentials{googleAnalytics,googleReCaptcha}
              }
            , systemEnvSalt
            } <- lift $ lift getSystemEnv

          -- Google Analytics
          script_ [async_ "", src_ $ printURI $ googleAnalyticsGTagToURI googleAnalytics] ("" :: T.Text)
          script_ [src_ $ printURI googleReCaptchaAssetURI] ("" :: T.Text)
          script_ [] $ renderJavascriptUrl (\_ _ -> undefined) $ googleAnalyticsScript googleAnalytics

          -- env <- lift (lift getEnv)
        
          -- ServerToClient
          let serverToClient = ServerToClient
                { serverToClientDevelopment = isDevelopment env
                , serverToClientFacebookClientId = clientId
                , serverToClientGoogleReCaptchaSiteKey = googleReCaptcha
                , serverToClientSalt = systemEnvSalt
                , serverToClientConfirmEmail = mConfEmail
                , serverToClientAuthToken = preliminary
                , serverToClientFormData = formData
                }
          script_ [] $ renderJavascriptUrl (\_ _ -> undefined) $ inlineScripts serverToClient
        }
  where
    inlineStyles = [lucius|
a:not([role="button"]), a:link:not([role="button"]), a:active:not([role="button"]) {
  color: #{localCookingColorsActive};
}
a:hover:not([role="button"]) {
  color: #{localCookingColorsHover};
}
a:visited:not([role="button"]) {
  color: #{localCookingColorsMain};
}
body {
  background-color: #{localCookingColorsMain} !important;
  padding-bottom: 5em;
}

.loader {
  border-radius: 50%;
  color: #ffffff;
  font-size: 11px;
  text-indent: -99999em;
  margin: 55px auto;
  position: relative;
  width: 10em;
  height: 10em;
  box-shadow: inset 0 0 0 1em;
  -webkit-transform: translateZ(0);
  -ms-transform: translateZ(0);
  transform: translateZ(0);
}
.loader:before,
.loader:after {
  position: absolute;
  content: '';
}
.loader:before {
  width: 6em;
  height: 12em;
  background: #{localCookingColorsMain};
  top: -1em;
  left: -1em;
  -webkit-transform-origin: 6em 6em;
  transform-origin: 6em 6em;
  -webkit-animation: load2 2s infinite ease 1.5s;
  animation: load2 2s infinite ease 1.5s;
}
.loader:after {
  width: 6em;
  height: 12em;
  background: #{localCookingColorsMain};
  top: -1em;
  left: 6em;
  -webkit-transform-origin: 0px 6em;
  transform-origin: 0px 6em;
  -webkit-animation: load2 2s infinite ease;
  animation: load2 2s infinite ease;
}
@-webkit-keyframes load2 {
  0% {
    -webkit-transform: rotate(0deg);
    transform: rotate(0deg);
  }
  100% {
    -webkit-transform: rotate(360deg);
    transform: rotate(360deg);
  }
}
@keyframes load2 {
  0% {
    -webkit-transform: rotate(0deg);
    transform: rotate(0deg);
  }
  100% {
    -webkit-transform: rotate(360deg);
    transform: rotate(360deg);
  }
}
|]

    inlineScripts serverToClient = [julius|
var serverToClient = #{Aeson.toJSON serverToClient}
|]

    googleAnalyticsScript gTag = [julius|
window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

gtag('config', #{Aeson.toJSON $ googleAnalyticsGTag gTag});
|]

-- | Inject some HTML into the @<body>@ tag of our template
mainTemplate :: LocalCookingSiteLinks siteLinks
             => Env
             -> LocalCookingColors
             -> Maybe ConfirmEmailError
             -> Maybe PreliminaryAuthToken
             -> Maybe FacebookLoginUnsavedFormData
             -> siteLinks
             -> HtmlT (AbsoluteUrlT SystemM) ()
             -> HtmlT (AbsoluteUrlT SystemM) ()
mainTemplate env colors mConfEmail preliminary formData =
  template . masterPage env colors mConfEmail preliminary formData



-- | Represents the resources used locally - @index.js@ and @index.css@.
data WebAssetLinks
  = IndexCss -- FIXME Cache buster
  | IndexJs (Maybe Nonce)

instance ToPath WebAssetLinks Abs File where
  toPath x = case x of
    IndexCss  -> [absfile|/index.css|]
    IndexJs _ -> [absfile|/index.js|]

instance ToLocation WebAssetLinks where
  toLocation x = case x of
    IndexCss -> fromAbsFile (toPath x)
    IndexJs mNonce ->
      let y = fromAbsFile (toPath x)
      in  case mNonce of
            Nothing -> y
            Just nonce -> addQuery ("cache_buster", Just $ BS8.toString $ BS16.encode $ NaCl.encode nonce) y
