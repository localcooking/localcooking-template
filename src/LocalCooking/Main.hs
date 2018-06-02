{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  , FlexibleContexts
  #-}

{-|

Module: LocalCooking.Main
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

This is the top-level entry point for a Local Cooking server - use the
'defaultMain' function to start the server, with custom fields defined in 'LocalCooking.Server.LocalCookingArgs'.

-}


module LocalCooking.Main where

import LocalCooking.Server (LocalCookingArgs, server)
import LocalCooking.Types (AppM, runAppM)
import LocalCooking.Types.Env (Env (..), defManagers, defDevelopment, defTokenContexts, releaseEnv)
import LocalCooking.Links.Class (LocalCookingSiteLinks)
import LocalCooking.Database.Query.Salt (getPasswordSalt)
import LocalCooking.Database.Schema (migrateAll)

import Options.Applicative (Parser, execParser, info, helper, fullDesc, progDesc, header, strOption, option, switch, auto, long, help, value, showDefault)
import qualified Data.Text as T
import Data.Attoparsec.Text (parseOnly)
import Data.Attoparsec.Path (absFilePath)
import Data.URI.Auth (parseURIAuth, URIAuth (..))
import Data.URI.Auth.Host (parseURIAuthHost)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS8
import Data.Monoid ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.Strict.Maybe as Strict
import Data.Insert.Class (Insertable)
import qualified Data.HashMap.Strict as HashMap
import Control.Applicative (Alternative)
import Control.Monad (unless)
import Control.Concurrent.STM (atomically, newTVarIO)
import Control.Logging (errorL, withStderrLogging)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Exception.Safe (bracket)
import Path (toFilePath, parent)
import Path.Extended (ToLocation, FromLocation)
import System.Directory (doesDirectoryExist, createDirectory)
import System.Environment (getEnv)
import Database.Persist.Sql (runSqlPool, runMigration)
import Database.Persist.Postgresql (createPostgresqlPool)


-- | Data type representing the raw arguments passed into the CLI executable
data ArgsImpl = ArgsImpl
  { argsImplSecretKey  :: FilePath -- ^ Path to the @~/.localcooking/secret@ file
  , argsImplHostname   :: String -- ^ Bound name and port of server - @localcooking.com:3000@
  , argsImplPublicPort :: Int -- ^ The port assumed to be represented by a reverse HTTP proxy - @80@
  , argsImplProduction :: Bool -- ^ \"In production\" or not
  , argsImplTls        :: Bool -- ^ If HTTP proxy supports TLS/SSL
  , argsImplDbHost     :: String -- ^ PostgreSQL host
  , argsImplDbPort     :: Int -- ^ PostgreSQL port
  , argsImplDbUser     :: String -- ^ PostgreSQL user
  , argsImplDbPassword :: String -- ^ PostgreSQL password
  , argsImplDbName     :: String -- ^ PostgreSQL database name
  }


-- | Arguments parser - marshalls CLI arguments into the 'ArgsImpl' data type.
args :: String -- ^ Username of process owner
     -> Parser ArgsImpl
args username = ArgsImpl
             <$> parseSecretKey
             <*> parseHostname
             <*> parsePublicPort
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



-- | Marshalls the 'ArgsImpl' data into functional references and shared resource pools
-- in 'LocalCooking.Types.Env.Env', and the port to bind to.
mkSystemEnv :: ArgsImpl -> IO (NewSystemEnvArgs, Int)
mkSystemEnv
  ArgsImpl
    { argsImplSecretKey
    , argsImplHostname
    , argsImplPublicPort
    , argsImplProduction
    , argsImplTls
    , argsImplDbHost
    , argsImplDbUser
    , argsImplDbPassword
    , argsImplDbPort
    , argsImplDbName
    } = do
  keys <- case parseOnly absFilePath (T.pack argsImplSecretKey) of
    Left e -> errorL $ "Secret key path not absolute: " <> T.pack e
    Right f -> do
      exists <- doesDirectoryExist $ toFilePath $ parent f
      unless exists $ createDirectory $ toFilePath $ parent f
      x <- LBS.readFile (toFilePath f)
      case Aeson.eitherDecode x of
        Left e -> errorL $ "Secret key file contents cannot be parsed: " <> T.pack e
        Right y -> pure (y :: Keys)

  (publicHostname, boundPort) <- case parseOnly parseURIAuth (T.pack argsImplHostname) of
    Left e -> errorL $ "Can't parse hostname: " <> T.pack e
    Right (URIAuth auth host mPort) -> pure $
      let port
            | argsImplPublicPort == 80 = Strict.Nothing
            | otherwise = Strict.Just (fromIntegral argsImplPublicPort)
      in  ( URIAuth auth host port
          , case mPort of
              Strict.Nothing -> 80
              Strict.Just p -> p
          )

  let newSystemEnvArgs = NewSystemEnvArgs
        { dbHost = T.pack argsImplDbHost
        , dbPort = argsImplDbPort
        , dbUser = T.pack argsImplDbUser
        , dbPassword = T.pack argsImplDbPassword
        , dbName = T.pack argsImplDbName
        , keys
        , facebookRedirect = URI
            (Strict.Just $ if argsImplTls "https" else "http")
            True
            publicHostname
            ["facebookLoginReturn"] -- TODO ensure route exists
            []
            Strict.Nothing
        }

  -- migrateAll systemEnvDatabase

  putStrLn $ unlines
    [ "Starting server with environment:"
    , " - hostname: " <> argsImplHostname
    , " - public port: " <> show argsImplPublicPort
    , " - database location: " <> argsImplDbHost <> ":" <> show argsImplDbPort
    , " - database user: " <> argsImplDbUser <> ":" <> argsImplDbPassword
    , " - secret key location: " <> argsImplSecretKey
    ]

  pure
    ( newSystemEnvArgs
    , fromIntegral boundPort
    )



-- | Top-level entry point to a Local Cooking server.
defaultMain :: LocalCookingSiteLinks siteLinks
            => FromLocation siteLinks
            => ToLocation siteLinks
            => String -- ^ CLI Invocation heading - i.e. \"@Local Cooking Farms - farm.localcooking.com daemon@\"
            -> LocalCookingArgs siteLinks sec [] -- ^ Arguments
            -> IO ()
defaultMain head' lcArgs = do
  username <- getEnv "USER"
  cliArgs <- execParser (opts username)
  (envArgs,boundPort) <- mkSystemEnv cliArgs

  withStderrLogging $
    execAppM
      envArgs
      (\SystemEnv{systemEnvDatabase} -> migrateAll systemEnvDatabase)
      (server port lcArgs)

  where
    opts u = info (helper <*> args u) $ fullDesc <> progDesc desc <> header head'
    desc = "Start the daemon"
