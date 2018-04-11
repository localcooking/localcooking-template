{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Facebook.State where


import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=), (.:), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Path (Abs, File, Dir)
import Path.Extended (ToLocation (toLocation), FromLocation (parseLocation), locationParser, printLocation)


data FacebookLoginState siteLinks = FacebookLoginState
  { facebookLoginStateOrigin :: siteLinks
  }

instance ToLocation siteLinks => ToJSON (FacebookLoginState siteLinks) where
  toJSON FacebookLoginState{..} = object
    [ "origin" .= (String $ printLocation $ toLocation facebookLoginStateOrigin)
    ]

instance FromLocation siteLinks => FromJSON (FacebookLoginState siteLinks) where
  parseJSON json = case json of
    Object o -> do
      json' <- o .: "origin"
      loc <- attoAeson locationParser json'
      case parseLocation loc of
        Left _ -> fail
        Right x -> pure (FacebookLoginState x)
    _ -> fail
    where
      fail = typeMismatch "FacebookLoginState" json
