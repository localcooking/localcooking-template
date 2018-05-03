{-|

Module: LocalCooking.Colors
Copyright: (c) 2018 Local Cooking Inc.
License: Proprietary
Maintainer: athan.clark@localcooking.com
Portability: GHC

-}

module LocalCooking.Colors where

import Text.Lucius (Color)


-- | The 3 primary colors, ideally obtained from the <https://material.io/color nifty Material.io color palette tool>.
data LocalCookingColors = LocalCookingColors
  { localCookingColorsMain   :: Color
  , localCookingColorsHover  :: Color
  , localCookingColorsActive :: Color
  }
