{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  #-}

{-|

Module: LocalCooking.Types.Env
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

Shared read-only data, across the server.

-}

module LocalCooking.Types.Env where

import LocalCooking.Server.Dependencies.AccessToken.Generic (AccessTokenContext, newAccessTokenContext)
import LocalCooking.Types.Keys (Keys)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Database.Schema.User (UserId)

import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (..))
import Data.Default (Default (..))
import qualified Data.Strict.Maybe as Strict
import Data.Pool (destroyAllResources)
import Control.Concurrent.STM (STM, atomically)
import Crypto.Saltine.Core.Box (Nonce, newNonce)
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Database.Persist.Sql (ConnectionPool)




-- | Read-only \"Environment\"
data Env = Env
  { envHostname        :: URIAuth -- ^ As bound
  , envSMTPHost        :: URIAuthHost -- ^ Remotely accessible mailer
  , envDevelopment     :: Maybe Development -- ^ Whether or not in \"development-mode\"
  , envTls             :: Bool -- ^ Served over TLS
  , envKeys            :: Keys -- ^ Parsed 'LocalCooking.Types.Keys.Keys' from @~/.localcooking/secret@
  , envManagers        :: Managers -- ^ Outgoing HTTP client managers
  , envDatabase        :: ConnectionPool -- ^ PostgreSQL connection pool
  , envSalt            :: HashedPassword -- ^ Persisted, long-term password salt
  , envTokenContexts   :: TokenContexts -- ^ TimeMap for decaying access tokens
  }

instance Default Env where
  def = Env
    { envHostname      = URIAuth Strict.Nothing Localhost (Strict.Just 3000)
    , envSMTPHost      = Localhost
    , envDevelopment   = def
    , envTls           = False
    , envKeys          = error "No access to secret keys in default environment"
    , envManagers      = def
    , envDatabase      = error "No database"
    , envSalt          = error "No salt"
    , envTokenContexts = def
    }

-- | Disconnects and cleans up safely
releaseEnv :: Env -> IO ()
releaseEnv Env{envDatabase} =
  destroyAllResources envDatabase


-- * Support Data Types

-- | HTTP managers
data Managers = Managers
  { managersFacebook  :: Manager
  , managersReCaptcha :: Manager
  }

instance Default Managers where
  def = unsafePerformIO defManagers

defManagers :: IO Managers
defManagers = do
  managersFacebook <- newTlsManager -- FIXME could bug out from facebook booting us
  managersReCaptcha <- newTlsManager
  pure Managers
    { managersFacebook
    , managersReCaptcha
    }

-- | Contains a site-wide \"cache buster\", so browsers don't use old assets
data Development = Development
  { devCacheBuster :: Nonce
  }

instance Default Development where
  def = unsafePerformIO defDevelopment

defDevelopment :: IO Development
defDevelopment = do
  devCacheBuster <- newNonce
  pure Development
    { devCacheBuster
    }

isDevelopment :: Env -> Bool
isDevelopment Env{envDevelopment} = case envDevelopment of
  Nothing -> False
  Just _ -> True


-- | Access token contexts, for expiring references
data TokenContexts = TokenContexts
  { tokenContextAuth :: AccessTokenContext AuthToken UserId
  }

instance Default TokenContexts where
  def = unsafePerformIO (atomically defTokenContexts)

defTokenContexts :: STM TokenContexts
defTokenContexts = do
  tokenContextAuth <- newAccessTokenContext
  pure TokenContexts
    { tokenContextAuth
    }
