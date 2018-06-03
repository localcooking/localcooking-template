{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , NamedFieldPuns
  #-}

module LocalCooking.Types
  ( Development, showCacheBuster, Env (..), isDevelopment
  ) where

import Data.URI (URI)
import Data.Functor.Identity (Identity)
import Data.Functor.Compose (Compose)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO (..))
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Trans.Control.Aligned as Aligned
import Control.Monad.Trans.Unlift (MonadBaseUnlift)
import Control.Monad.Reader (ReaderT, ask)
import Crypto.Saltine.Core.Box (Nonce)
import qualified Crypto.Saltine.Class as NaCl
import Path.Extended (Location)



data Development = Development
  { devCacheBuster :: Nonce
  }

showCacheBuster :: Development -> ByteString
showCacheBuster Development{devCacheBuster} = NaCl.encode devCacheBuster


data Env = Env
  { envMkURI :: Location -> URI
  , envDevelopment :: Maybe Development
  }

isDevelopment :: Env -> Bool
isDevelopment Env{envDevelopment} = case envDevelopment of
  Nothing -> False
  Just _ -> True
