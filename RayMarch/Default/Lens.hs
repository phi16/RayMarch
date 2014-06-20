module RayMarch.Default.Lens where

import RayMarch.Types

defaultLens :: Pixel -> Vector
defaultLens (x,y) = Vector (1,x,y)

fisheyeLens :: Float -> Pixel -> Vector
fisheyeLens f p = let
    (x,y) = p<*>(f/90)
    s = sqrt $ x^2+y^2
    a = pi/2*s
  in Vector (cos a, sin a*x/s, sin a*y/s)
