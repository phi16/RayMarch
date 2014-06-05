module RayMarch.Default.Effector where

import RayMarch.Types

noEffect :: Point -> Config -> Pixel -> Color -> Color
noEffect p cfg px c = c

-- fogEffect :: Point -> Config -> Pixel -> Color -> Color
 