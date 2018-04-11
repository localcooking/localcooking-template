{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , NamedFieldPuns
  , QuasiQuotes
  , StandaloneDeriving
  , MultiParamTypeClasses
  #-}

module LocalCooking.Template where

import           LocalCooking.Types (AppM)
import           LocalCooking.Types.Env (Env (..), Development (..), isDevelopment)
import           LocalCooking.Types.FrontendEnv (FrontendEnv (..))
import           LocalCooking.Types.Keys (Keys (..))
import           LocalCooking.Auth.Error (AuthError, PreliminaryAuthToken (..))
import           Facebook.App (Credentials (..))
import           Google.Keys (GoogleCredentials (..))
import           Google.Analytics (googleAnalyticsGTagToURI, GoogleAnalyticsGTag (..))
import           Google.ReCaptcha (googleReCaptchaAssetURI)
import           LocalCooking.Common.AuthToken (AuthToken)

import           Lucid (renderBST, HtmlT, Attribute, content_, name_, meta_, httpEquiv_, charset_, link_, rel_, type_, href_, sizes_, script_, src_, async_)
import           Lucid.Base (makeAttribute)
import           Network.HTTP.Types (Status, status200)
import qualified Network.Wai.Middleware.ContentType.Types as CT
import           Web.Page.Lucid (template, WebPage (..))
import           Web.Routes.Nested (FileExtListenerT, mapHeaders, mapStatus, bytestring)

import qualified Data.Text                                as T
import qualified Data.Text.Encoding                       as T
import qualified Data.Text.Lazy.Encoding                  as LT
import           Data.Default
import qualified Data.HashMap.Strict                      as HM
import           Data.Markup                              as M
import           Data.Url (AbsoluteUrlT (..), packLocation)
import           Data.URI (URI (..), printURI)
import           Data.URI.Auth (URIAuth (..))
import           Data.URI.Auth.Host (URIAuthHost (..))
import           Data.Aeson (ToJSON (..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Base16                   as BS16
import qualified Data.ByteString.UTF8                     as BS8
import qualified Data.ByteString.Lazy                     as LBS
import qualified Data.Strict.Maybe                        as Strict
import           Data.Monoid ((<>))
import           Text.Heredoc (here)
import           Control.Monad.Trans                      (lift)
import           Control.Monad.Reader                     (ask)
import           Control.Monad.State                      (modify)
import           Control.Monad.Trans                      (lift)
import           Control.Monad.Morph                      (hoist)
import           Path.Extended (ToPath (..), ToLocation (..), Abs, File, fromPath, setFileExt, addQuery, parseAbsFile, parseAbsDir, absfile)
import           Text.Julius (julius, renderJavascriptUrl)
import           Text.Lucius (lucius, renderCssUrl, Color (..))
import           Crypto.Saltine.Core.Box (Nonce)
import qualified Crypto.Saltine.Class                     as NaCl


deriving instance Show URIAuthHost
deriving instance Show URIAuth
deriving instance Show URI


htmlLight :: Status
          -> HtmlT (AbsoluteUrlT AppM) a
          -> FileExtListenerT AppM ()
htmlLight s content = do
  bs <- lift $ do
    Env{envHostname,envTls} <- ask
    let locationToURI loc = packLocation (Strict.Just $ if envTls then "https" else "http") True envHostname loc
    runAbsoluteUrlT (renderBST content) locationToURI

  bytestring CT.Html bs
  modify . HM.map $ mapStatus (const s)
                  . mapHeaders ([("content-Type", "text/html")] ++)


html :: Maybe (Either AuthError AuthToken)
     -> HtmlT (AbsoluteUrlT AppM) ()
     -> FileExtListenerT AppM ()
html mToken = htmlLight status200 . mainTemplate mToken


masterPage :: Maybe (Either AuthError AuthToken)
           -> WebPage (HtmlT (AbsoluteUrlT AppM) ()) T.Text [Attribute]
masterPage mToken =
  let page :: WebPage (HtmlT (AbsoluteUrlT AppM) ()) T.Text [Attribute]
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
        , pageTitle = "Local Cooking"
        , styles =
          deploy M.Css Inline $ renderCssUrl (\_ _ -> undefined) inlineStyles
        , bodyScripts = do
          Env{envDevelopment = mDev} <- lift ask
          deploy M.JavaScript M.Remote $ toLocation $ IndexJs $ devCacheBuster <$> mDev
        , afterStylesScripts = do
          env@Env
            { envKeys = Keys
              { keysFacebook = Credentials{clientId}
              , keysGoogle = GoogleCredentials{googleAnalytics,googleReCaptcha}
              }
            , envSalt
            } <- lift ask

          -- Google Analytics
          script_ [async_ "", src_ $ printURI $ googleAnalyticsGTagToURI googleAnalytics] ("" :: T.Text)
          script_ [src_ $ printURI googleReCaptchaAssetURI] ("" :: T.Text)
          script_ [] $ renderJavascriptUrl (\_ _ -> undefined) $ googleAnalyticsScript googleAnalytics

          -- FrontendEnv
          let frontendEnv = FrontendEnv
                { frontendEnvDevelopment = isDevelopment env
                , frontendEnvFacebookClientID = clientId
                , frontendEnvGoogleReCaptchaSiteKey = googleReCaptcha
                , frontendEnvSalt = envSalt
                , frontendEnvAuthToken = PreliminaryAuthToken mToken
                }
          script_ [] $ renderJavascriptUrl (\_ _ -> undefined) $ inlineScripts frontendEnv
        }
  where
    inlineStyles = [lucius|
a:link:not([role="button"]), a:active:not([role="button"]) {
  color: #{aLinkActive};
}
a:hover:not([role="button"]) {
  color: #{aHover};
}
a:visited:not([role="button"]) {
  color: #{aVisited};
}
body {
  background-color: #{background} !important;
  padding-bottom: 5em;
}|]
      where
        aLinkActive = Color 198 40 40
        aHover = Color 255 95 82
        aVisited = Color 142 0 0
        background = Color 142 0 0

    inlineScripts frontendEnv = [julius|
var frontendEnv = #{Aeson.toJSON frontendEnv}
|]

    googleAnalyticsScript gTag = [julius|
window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

gtag('config', #{Aeson.toJSON $ googleAnalyticsGTag gTag});
|]

-- | Inject some HTML into the @<body>@ tag of our template
mainTemplate :: Maybe (Either AuthError AuthToken)
             -> HtmlT (AbsoluteUrlT AppM) ()
             -> HtmlT (AbsoluteUrlT AppM) ()
mainTemplate = template . masterPage



data WebAssetLinks
  = IndexCss -- FIXME Cache buster
  | IndexJs (Maybe Nonce)

instance ToPath WebAssetLinks Abs File where
  toPath x = case x of
    IndexCss  -> [absfile|/index|]
    IndexJs _ -> [absfile|/index|]

instance ToLocation WebAssetLinks Abs File where
  toLocation x = case x of
    IndexCss -> setFileExt (Just "css") $ fromPath $ toPath x
    IndexJs mNonce ->
        setFileExt (Just "js")
      $ ( case mNonce of
            Nothing -> id
            Just nonce -> addQuery ("cache_buster", Just $ BS8.toString $ BS16.encode $ NaCl.encode nonce)
        )
      $ fromPath
      $ toPath x
