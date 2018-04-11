{-# LANGUAGE
    NamedFieldPuns
  , OverloadedLists
  , OverloadedStrings
  #-}

module LocalCooking.Main.Options where

import LocalCooking.Types.Env (Env (..), defManagers, defDevelopment)
import LocalCooking.Database.Query.Salt (getPasswordSalt)
import qualified LocalCooking.Database.Schema.Facebook as Facebook
import qualified LocalCooking.Database.Schema.User as User
import qualified LocalCooking.Database.Schema.Salt as Salt

import Options.Applicative (Parser, strOption, option, switch, auto, long, help, value, showDefault)
import Data.Attoparsec.Text (parseOnly)
import Data.Attoparsec.Path (absFilePath)
import Data.URI.Auth (parseURIAuth, URIAuth (..))
import Data.URI.Auth.Host (parseURIAuthHost)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS8
import Data.Monoid ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.Strict.Maybe as Strict
import Data.TimeMap (newTimeMap)
import Control.Monad (unless)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMapMVar.Hash (newTMapMVar)
import Control.Logging (errorL)
import Control.Monad.Logger (runStderrLoggingT)
import Path (toFilePath, parent)
import System.Directory (doesDirectoryExist, createDirectory)
import Database.Persist.Sql (runSqlPool, runMigration)
import Database.Persist.Postgresql (createPostgresqlPool)


data ArgsImpl = ArgsImpl
  { argsImplSecretKey  :: FilePath
  , argsImplHostname   :: String
  , argsImplPublicPort :: Int
  , argsImplSMTPHost   :: String
  , argsImplProduction :: Bool
  , argsImplTls        :: Bool
  , argsImplDbHost     :: String
  , argsImplDbPort     :: Int
  , argsImplDbUser     :: String
  , argsImplDbPassword :: String
  , argsImplDbName     :: String
  }


args :: String -> Parser ArgsImpl
args username = ArgsImpl
             <$> parseSecretKey
             <*> parseHostname
             <*> parsePublicPort
             <*> parseSMTPHost
             <*> parseProduction
             <*> parseTls
             <*> parseDbHost
             <*> parseDbPort
             <*> parseDbUser
             <*> parseDbPassword
             <*> parseDbName
  where
    parseSecretKey = strOption $
      long "secret-key" <> help "File path to the properly accessible secrets file, containing the PayPal API tokens, etc."
        <> value ("/home/" ++ username ++ "/.localcooking/secret") <> showDefault
    parseHostname = strOption $
      long "host" <> help "Bound name & port of the server (for hyperlinks)"
        <> value "localhost:3000" <> showDefault
    parsePublicPort = option auto $
      long "public-port" <> help "Publically accessible port of the service, if different from the bound port (i.e. 80)"
        <> value 3000 <> showDefault
    parseSMTPHost = strOption $
      long "smtp-host" <> help "Hostname of the SMTP outgoing mail server"
        <> value "localhost" <> showDefault
    parseProduction = switch $
      long "production" <> help "Run the server in production-mode (less logging)"
    parseTls = switch $
      long "tls" <> help "Assume the server is running behind a TLS HTTP proxy"
    parseDbHost = strOption $
      long "db-host" <> help "Hostname of the PostgreSQL database"
        <> value "localhost" <> showDefault
    parseDbPort = option auto $
      long "db-port" <> help "Port of the PostgreSQL database"
        <> value 5432 <> showDefault
    parseDbUser = strOption $
      long "db-user" <> help "User for the PostgreSQL database"
    parseDbPassword = strOption $
      long "db-password" <> help "Password for the PostgreSQL database"
    parseDbName = strOption $
      long "db-name" <> help "Database name for the PostgreSQL pooled connection"


mkEnv :: ArgsImpl -> IO (Env, Int)
mkEnv
  ArgsImpl
    { argsImplSecretKey
    , argsImplHostname
    , argsImplPublicPort
    , argsImplSMTPHost
    , argsImplProduction
    , argsImplTls
    , argsImplDbHost
    , argsImplDbUser
    , argsImplDbPassword
    , argsImplDbPort
    , argsImplDbName
    } = do
  envKeys <- case parseOnly absFilePath (T.pack argsImplSecretKey) of
    Left e -> errorL $ "Secret key path not absolute: " <> T.pack e
    Right f -> do
      exists <- doesDirectoryExist $ toFilePath $ parent f
      unless exists $ createDirectory $ toFilePath $ parent f
      x <- LBS.readFile (toFilePath f)
      case Aeson.eitherDecode x of
        Left e -> errorL $ "Secret key file contents cannot be parsed: " <> T.pack e
        Right y -> pure y
  (envHostname, boundPort) <- case parseOnly parseURIAuth (T.pack argsImplHostname) of
    Left e -> errorL $ "Can't parse hostname: " <> T.pack e
    Right (URIAuth a h mPort) -> pure
      ( URIAuth a h ( if argsImplPublicPort == 80
                      then Strict.Nothing
                      else Strict.Just $ fromIntegral argsImplPublicPort
                    )
      , case mPort of
          Strict.Nothing -> 80
          Strict.Just p -> p
      )
  envSMTPHost <- case parseOnly parseURIAuthHost (T.pack argsImplSMTPHost) of
    Left e -> errorL $ "Can't parse SMTP host: " <> T.pack e
    Right a -> pure a

  putStrLn $ unlines
    [ "Starting server with environment:"
    , " - hostname: " <> argsImplHostname
    , " - public port: " <> show argsImplPublicPort
    , " - database location: " <> argsImplDbHost <> ":" <> show argsImplDbPort
    , " - database user: " <> argsImplDbUser <> ":" <> argsImplDbPassword
    , " - secret key location: " <> argsImplSecretKey
    ]

  envManagers <- defManagers
  envDevelopment <- if argsImplProduction then pure Nothing else Just <$> defDevelopment

  envDatabase <- do
    let connStr = "host=" <> BS8.fromString argsImplDbHost
               <> " port=" <> BS8.fromString (show argsImplDbPort)
               <> " user=" <> BS8.fromString argsImplDbUser
               <> " password=" <> BS8.fromString argsImplDbPassword
               <> " dbname=" <> BS8.fromString argsImplDbName
    runStderrLoggingT (createPostgresqlPool connStr 10)

  flip runSqlPool envDatabase $ do
    runMigration Facebook.migrateAll
    runMigration User.migrateAll
    runMigration Salt.migrateAll

  envSalt <- getPasswordSalt envDatabase

  envAuthTokens <- atomically newTimeMap

  envAuthTokenExpire <- atomically newTMapMVar

  pure
    ( Env
      { envHostname
      , envSMTPHost
      , envDevelopment
      , envTls = argsImplTls
      , envKeys
      , envManagers
      , envDatabase
      , envSalt
      , envAuthTokens
      , envAuthTokenExpire
      }
    , fromIntegral boundPort
    )
