{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  #-}

module LocalCooking.Server.Assets where

import Data.FileEmbed (embedFile, embedDir)
import Data.ByteString (ByteString)


privacyPolicy :: ByteString
privacyPolicy = $(embedFile "./privacypolicy.htm")
