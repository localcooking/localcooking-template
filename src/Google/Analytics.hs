{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , OverloadedLists
  , GeneralizedNewtypeDeriving
  #-}

module Google.Analytics where

import Data.Aeson (FromJSON)
import Data.URI (URI (..))
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (Host))
import Data.Text (Text)
import qualified Data.Strict.Maybe as Strict
import Data.Strict.Tuple (Pair (..))


newtype GoogleAnalyticsGTag = GoogleAnalyticsGTag
  { googleAnalyticsGTag :: Text
  } deriving (Eq, Show, FromJSON)


googleAnalyticsGTagToURI :: GoogleAnalyticsGTag -> URI
googleAnalyticsGTagToURI GoogleAnalyticsGTag{..} =
  URI
    (Strict.Just "https")
    True
    (URIAuth Strict.Nothing (Host ["www","googletagmanager"] "com") Strict.Nothing)
    ["gtag", "js"]
    ["id" :!: Strict.Just googleAnalyticsGTag]
    Strict.Nothing
