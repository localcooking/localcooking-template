module LocalCooking.Colors where

import Text.Lucius (Color)

data LocalCookingColors = LocalCookingColors
  { localCookingColorsMain   :: Color
  , localCookingColorsHover  :: Color
  , localCookingColorsActive :: Color
  }
