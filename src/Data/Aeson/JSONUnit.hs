{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.Aeson.JSONUnit where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)


data JSONUnit = JSONUnit
  deriving (Eq, Show)

instance ToJSON JSONUnit where
  toJSON JSONUnit = String ""

instance FromJSON JSONUnit where
  parseJSON json = case json of
    String x
      | x == "" -> pure JSONUnit
      | otherwise -> fail
    _ -> fail
    where
      fail = typeMismatch "JSONUnit" json
