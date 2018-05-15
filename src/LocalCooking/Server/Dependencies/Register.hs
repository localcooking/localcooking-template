{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , RecordWildCards
  #-}

module LocalCooking.Server.Dependencies.Register where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..), Managers (..))
import LocalCooking.Types.Keys (Keys (..))
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Email (EmailToken (..))
import LocalCooking.Common.AccessToken (genAccessToken)
import LocalCooking.Database.Query.User (registerUser, registerFBUserId, RegisterFailure)
import Google.Keys (ReCaptchaResponse (getReCaptchaResponse), ReCaptchaSecret (getReCaptchaSecret), ReCaptchaVerifyResponse (..), googleReCaptchaVerifyURI, GoogleCredentials (..))
import SparkPost.Keys (SparkPostCredentials (..), showSparkPostKey, confirmEmailRequest)
import Facebook.Types (FacebookUserId)

import Text.EmailAddress (EmailAddress)
import qualified Text.EmailAddress as EmailAddress
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.=), (.:), Value (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import Data.URI (printURI)
import Data.URI.Auth.Host (printURIAuthHost)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative (Alternative)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
import Control.Logging (log')
import Control.Concurrent.STM (atomically, modifyTVar)
import Network.HTTP.Client (httpLbs, responseBody, parseRequest, method, urlEncodedBody, requestHeaders)
import Network.Mail.SMTP (sendMail, simpleMail, htmlPart, Address (..))

import Web.Dependencies.Sparrow.Types (Server, staticServer, JSONVoid)
import Lucid (renderTextT)
import qualified Lucid.Html5 as L


data RegisterInitIn = RegisterInitIn
  { registerInitInEmail     :: EmailAddress
  , registerInitInPassword  :: HashedPassword
  , registerInitInReCaptcha :: ReCaptchaResponse
  , registerInitInFbUserId  :: Maybe FacebookUserId
  }


instance FromJSON RegisterInitIn where
  parseJSON json = case json of
    Object o -> RegisterInitIn <$> o .: "email"
                               <*> o .: "password"
                               <*> o .: "reCaptcha"
                               <*> o .: "fbUserId"
    _ -> fail
    where fail = typeMismatch "RegisterInitIn" json



data RegisterInitOut
  = RegisterInitOutEmailSent
  | RegisterInitOutBadCaptcha
  | RegisterInitOutDBError RegisterFailure


instance ToJSON RegisterInitOut where
  toJSON x = case x of
    RegisterInitOutEmailSent -> String "email-sent"
    RegisterInitOutBadCaptcha -> String "bad-captcha"
    RegisterInitOutDBError e -> object ["db" .= e]



registerServer :: Alternative f
               => Server AppM f RegisterInitIn
                                RegisterInitOut
                                JSONVoid
                                JSONVoid
registerServer = staticServer $ \RegisterInitIn{..} -> do
  liftIO $ log' "running..."

  Env
    { envManagers = Managers{managersReCaptcha,managersSparkPost}
    , envKeys = Keys
      { keysGoogle = GoogleCredentials{googleReCaptchaSecret}
      , keysSparkPost = SparkPostCredentials{sparkPostKey}
      }
    , envDatabase
    , envPendingEmail
    } <- ask

  liftIO $ do
    req <- parseRequest $ T.unpack $ printURI googleReCaptchaVerifyURI

    let -- body = Aeson.encode $ ReCaptchaVerify googleReCaptchaSecret registerInitInReCaptcha
        req' = urlEncodedBody
                 [ ("secret", T.encodeUtf8 $ getReCaptchaSecret googleReCaptchaSecret)
                 , ("response", T.encodeUtf8 $ getReCaptchaResponse registerInitInReCaptcha)
                 ]
                 $ req
                    { method = "POST"
                    }

    log' "sending..."

    resp <- httpLbs req' managersReCaptcha

    case Aeson.decode (responseBody resp) of
      Nothing -> do
        liftIO $ log' $ "Couldn't parse response body: " <> T.pack (show $ responseBody resp)
        pure Nothing
      Just (ReCaptchaVerifyResponse success)
        | not success -> do
          liftIO $ log' $ "recaptcha failure: " <> T.pack (show $ responseBody resp)
          pure $ Just RegisterInitOutBadCaptcha
        | otherwise -> do
            eUid <- liftIO $
              registerUser envDatabase registerInitInEmail registerInitInPassword
            case eUid of
              Left e -> do
                log' "db error"
                pure $ Just $ RegisterInitOutDBError e
              Right uid -> do
                resp <- liftIO $ do
                  case registerInitInFbUserId of
                    Nothing -> pure ()
                    Just fbUserId -> registerFBUserId envDatabase uid fbUserId
                  emailToken <- EmailToken <$> genAccessToken
                  atomically $ modifyTVar envPendingEmail $ HashMap.insert emailToken uid
                  req <- confirmEmailRequest sparkPostKey registerInitInEmail emailToken
                  log' $ "sending email..." <> T.pack (show req)
                  httpLbs req managersSparkPost
                log' $ "Email Sent: " <> T.pack (show resp)
                pure $ Just RegisterInitOutEmailSent
