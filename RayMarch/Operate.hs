module RayMarch.Operate where

import RayMarch.Types
import RayMarch.March

smooth :: Float
smooth = 0.1

clamp :: Float -> (Float,Float) -> Float
clamp x (a,b) 
  | x < a = a
  | x > b = b
  | otherwise = x

mix :: Float -> Float -> Float -> Float
mix r x y = x*(1-r)+y*r

smin :: Float -> Float -> Float
smin a b = let
    h = clamp (0.5+0.5*(b-a)/smooth) (0,1)
  in mix h a b - smooth*h*(1-h)

reflectOn :: Vector -> Vector -> Vector
reflectOn v n = n<*>(-2*n`dot`v) <+> v

black :: Color
black = Color (0,0,0)

white :: Color
white = Color (1,1,1)

gray :: Float -> Color
gray r = Color (r,r,r)

normal :: Point -> March Vector
normal p = do
  (x1,_) <- distance $ p <+> Vector (eps,0,0)
  (x2,_) <- distance $ p <-> Vector (eps,0,0)
  (y1,_) <- distance $ p <+> Vector (0,eps,0)
  (y2,_) <- distance $ p <-> Vector (0,eps,0)
  (z1,_) <- distance $ p <+> Vector (0,0,eps)
  (z2,_) <- distance $ p <-> Vector (0,0,eps)
  return $ norm $ inv $ Vector (x1-x2,y1-y2,z1-z2)
