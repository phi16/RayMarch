module RayMarch.Primitive where

import RayMarch.Types

sphere :: Object -> Float -> Object -> Distance
sphere o r p = (len p - r, o)

box :: Object -> Vector -> Distance
box o v p = let
    d = each abs p |-| v
  in (0`min`fold max d + len (each (max 0) d), o)

plane :: Object -> Vector -> Float -> Distance
plane o v d p = (p`dot`v+d, o)
