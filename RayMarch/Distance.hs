module RayMarch.Distance where

import RayMarch.Types
import RayMarch.Quaternion
import RayMarch.Operate

(<&>) :: Distance s -> Distance s -> Distance s
(x <&> y) p = let
    a@(ad,ao) = x p
    b@(bd,bo) = y p
  in if ad > bd then a else b

(<|>) :: Distance s -> Distance s -> Distance s
(x <|> y) p = let
    a@(ad,ao) = x p
    b@(bd,bo) = y p
  in if ad < bd then a else b

(<~>) :: Distance s -> Distance s -> Distance s
(x <~> y) p = let
    (ad,ao) = x p
    (bd,bo) = y p
    d = ad`smin`bd
    r = clamp (0.5+0.5*(bd-ad)/smooth) (0,1)
    obj p v = do
      a <- ao p v
      b <- bo p v
      return $ lerp r b a
  in (d,obj)

(<\>) :: Distance s -> Distance s -> Distance s
(x <\> y) p = let
    a@(ad,ao) = x p
    b@(bd,bo) = y p
  in (ad`max`(-bd),ao)

invert :: Distance s -> Distance s
invert d p = let
    (l,o) = d p
  in (-l,o)
 
scale :: Float -> Distance s -> Distance s
scale s d p = let
    (l,o) = d (p </> s)  
  in (l*s, o)

transpose :: Vector -> Distance s -> Distance s
transpose v d p = d (p <-> v)

rotate :: Quaternion -> Distance s -> Distance s
rotate q d p = d $ apply (inverse q) p

displacement :: (Point -> Float) -> Distance s -> Distance s
displacement f d p = let (l,o) = d p in (l+f p,o)

repeatX :: Float -> Distance s -> Distance s
repeatX x d (Vector (p,q,r)) = d $ Vector (p`fmod`x-x/2,q,r)

repeatY :: Float -> Distance s -> Distance s
repeatY y d (Vector (p,q,r)) = d $ Vector (p,q`fmod`y-y/2,r)

repeatZ :: Float -> Distance s -> Distance s
repeatZ z d (Vector (p,q,r)) = d $ Vector (p,q,r`fmod`z-z/2)
