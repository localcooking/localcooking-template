module LocalCooking.Links.Class where

import Data.Text (Text)


class LocalCookingSiteLinks siteLinks where
  rootLink :: siteLinks
  registerLink :: siteLinks
  toDocumentTitle :: siteLinks -> Text
