{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Data.Insert.Class where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Coerce (Coercible, coerce)
import Control.Monad.IO.Class (MonadIO (liftIO))


class Insertable f m where
  insert :: a -> f a -> m (f a)

instance Applicative m => Insertable [] m where
  insert x xs = pure (x:xs)

instance (Coercible k UUID, Eq k, Hashable k, MonadIO m) => Insertable (HashMap k) m where
  insert x xs = do
    k <- liftIO nextRandom
    pure (HashMap.insert (coerce k) x xs)
