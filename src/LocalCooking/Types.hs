{-# LANGUAGE
    DeriveFunctor
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  , MultiParamTypeClasses
  , UndecidableInstances
  #-}

module LocalCooking.Types
  ( AppM
  ) where

import LocalCooking.Function.System (SystemM)

import Data.URI (URI)
import Data.Functor.Identity (Identity)
import Data.Functor.Compose (Compose)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO (..))
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.Trans.Control.Aligned as Aligned
import Control.Monad.Trans.Unlift (MonadBaseUnlift)
import Control.Monad.Reader (ReaderT)
import Crypto.Saltine.Core.Box (Nonce)
import Path.Extended (Location)



data Development = Development
  { devCacheBuster :: Nonce
  }


data Env = Env
  { envMkURI :: Location -> URI
  , envDevelopment :: Maybe Development
  }



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
