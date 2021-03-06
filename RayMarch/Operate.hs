module RayMarch.Operate where

import Data.Fixed (mod')
import RayMarch.Types
import RayMarch.March

type Dup a = (a,a)

eps :: Float
eps = 0.0001

delta :: Float
delta = 0.001

infinity :: Float
infinity = 1 / 0

smooth :: Float
smooth = 0.3

fmod :: Float -> Float -> Float
fmod = mod'

for :: [a] -> (a -> b) -> [b]
for = flip map

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 f x = x
iterateN c f x = iterateN (c-1) f (f x)

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
  in mix h b a - smooth*h*(1-h)

rad :: Float -> Float
rad x = x * pi / 180

reflectOn :: Vector -> Vector -> Vector
reflectOn v n = norm $ n<*>(-2*n`dot`v) <+> v

refractOn :: Float -> Vector -> Vector -> Maybe Vector
refractOn r v n = let
    d = v`dot`n
    u = 1 - r*r*(1-d*d)
    t = v<*>r <-> n<*>(r*d+sqrt u)
  in if u >= 0 then Just $ norm t else Nothing

black :: Color
black = Color (0,0,0)

white :: Color
white = Color (1,1,1)

gray :: Float -> Color
gray r = Color (r,r,r)

normal :: Point -> March s Vector
normal p = do
  (x1,_) <- distance $ p <+> Vector (eps,0,0)
  (x2,_) <- distance $ p <-> Vector (eps,0,0)
  (y1,_) <- distance $ p <+> Vector (0,eps,0)
  (y2,_) <- distance $ p <-> Vector (0,eps,0)
  (z1,_) <- distance $ p <+> Vector (0,0,eps)
  (z2,_) <- distance $ p <-> Vector (0,0,eps)
  return $ norm $ inv $ Vector (x1-x2,y1-y2,z1-z2)
