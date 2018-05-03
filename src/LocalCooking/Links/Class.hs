{-|

Module: LocalCooking.Links.Class
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Links.Class where

import Data.Text (Text)


-- | Generalizes a /@SiteLinks@/ type
class LocalCookingSiteLinks siteLinks where
  rootLink :: siteLinks
  registerLink :: siteLinks
  toDocumentTitle :: siteLinks -> Text
