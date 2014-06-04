module RayMarch.Operate where

import RayMarch.Types

smooth :: Float
smooth = 0.1

clamp :: Float -> (Float,Float) -> Float
clamp x (a,b) 
  x < a = a
  x > b = b
  otherwise = x

mix :: Float -> Float -> Float
mix x y r = x*(1-r)+y*r

smin :: Float -> Float -> Float
smin a b = let
    h = clamp (0.5+0.5*(b-a)/smooth) (0,1)
  in mix a b h - smooth*h*(1-h)

reflectOn :: Vector -> Vector -> Vector
reflectOn v n = n<*>(-2*n`dot`v) + v
