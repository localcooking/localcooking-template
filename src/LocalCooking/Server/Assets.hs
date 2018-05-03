{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  #-}

{-|

Module: LocalCooking.Server.Assets
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Server.Assets where

import Data.FileEmbed (embedFile)
import Data.ByteString (ByteString)


-- | Generic privacy policy
privacyPolicy :: ByteString
privacyPolicy = $(embedFile "./privacypolicy.htm")
