module RayMarch.Primitive where

import Control.Arrow hiding ((<+>))
import RayMarch.Types
import RayMarch.Operate

sphere :: Float -> Object s -> Distance s
sphere r o p = (len p - r, o)

box :: Vector -> Object s -> Distance s
box v o p = let
    d = each abs p <-> v 
  in (0`min`fold max d + len (each (max 0) d) - delta, o)

plane :: Vector -> Float -> Object s -> Distance s
plane v d o p = (p`dot`v+d, o)

fractal :: Float -> Point -> (Point -> Point) -> (Point -> Float) -> Object s -> Distance s
fractal z x f e o p = (e point * z**(-count), o) where
  (point,count) = until end (expand . f *** succ) (p,0)
  end (p,c) = c >= 2
  expand p = (p<->x)<*>z<+>x

type Plane = (Float,Float,Float,Float)

upper :: Plane -> Point -> Float
upper (a,b,c,d) (Vector (x,y,z)) = (a*x+b*y+c*z+d) / sqrt (a*a+b*b+c*c)

foldFractal :: Float -> Point -> [Plane] -> [Plane] -> Object s -> Distance s
foldFractal z x fs es o = fractal z x f e o where
  f p = foldr (\s p -> if upper s p >= 0 then refl s p else p) p fs
  refl s@(a,b,c,d) p@(Vector (x,y,z)) = p<->(Vector (a,b,c))<*>(a*x+b*y+c*z+d)</>(a*a+b*b+c*c)<*>2
  e p = maximum $ map (flip upper p) es

areaFractal :: Float -> [(Distance s, Point)] -> Object s -> Distance s
areaFractal z xs o p = (dist * z**(-fromIntegral count+1), o) where
  count = 4
  (_,(_,dist)) = iterateN count (expand . proc) (p, undefined)
  expand (p,(x,d)) = ((p<->x)<*>z<+>x, (x,d))
  proc (p,_) = (p, foldr run (p,infinity) xs) where
    run (f,x) (u,d) = let l = fst $ f p in if l < d then (x,l) else (u,d)

circle :: Float -> Field
circle r p = len p - r 

rect :: (Float,Float) -> Field
rect v p = let
    d = each abs p <-> v 
  in 0`min`fold max d + len (each (max 0) d) - delta

thickness :: Float -> Field -> Field
thickness w f m = f m - w

push :: Field -> Float -> Object s -> Distance s
push f d o (Vector (x,y,z)) = let
    r = f (x,y)
    b = abs z - d
  in (,o) $ case (r < 0, abs z > d) of
    (True,True) ->   b
    (True,False) ->  r`max`b
    (False,True) ->  sqrt $ b*b+r*r
    (False,False) -> r

