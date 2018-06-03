{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  , NamedFieldPuns
  #-}

module LocalCooking.Types
  ( AppM, Development, showCacheBuster, Env (..), liftSystem, isDevelopment, getEnv
  ) where

import LocalCooking.Function.System (SystemM)

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


newtype AppM a = AppM
  { getAppM :: ReaderT Env SystemM a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadBaseControl IO, MonadBaseUnlift IO)


instance MonadUnliftIO AppM where
  askUnliftIO = AppM $ do
    UnliftIO f <- askUnliftIO
    pure $ UnliftIO $ f . getAppM
  withRunInIO runner = AppM $ withRunInIO $ \fromM -> runner (fromM . getAppM)

instance Aligned.MonadBaseControl IO AppM (Compose (Compose Identity Identity) Identity) where
  liftBaseWith f = AppM $ Aligned.liftBaseWith $ \runInBase ->
    f (runInBase . getAppM)
  restoreM = AppM . Aligned.restoreM


liftSystem :: SystemM a -> AppM a
liftSystem x = AppM (lift x)


getEnv :: AppM Env
getEnv = AppM ask
