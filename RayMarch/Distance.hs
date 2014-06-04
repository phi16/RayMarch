module RayMarch.Distance where

import RayMarch.Types
import RayMarch.Quaternion
import RayMarch.Operate

(<&>) :: Distance -> Distance -> Distance
(x <&> y) p = let
    a@(ad,ao) = x p
    b@(bd,bo) = y p
  in if ad < bd then a else b

(<~>) :: Distance -> Distance -> Distance
(x <~> y) p = let
    (ad,ao) = x p
    (bd,bo) = y p
    d = ad`smin`bd
    r = (d-ad)/(bd-ad)
  in (d, \p v ->(ao p v)<*>(1-r)<+>(bo p v)<*>r)

(<|>) :: Distance -> Distance -> Distance
(x <|> y) p = let
    a@(ad,ao) = x p
    b@(bd,bo) = y p
  in if ad > bd then a else b

(<\>) :: Distance -> Distance -> Distance
(x <\> y) p = let
    a@(ad,ao) = x p
    b@(bd,bo) = y p
  in (ad`max`(-bd),ao)
 
scale :: Float -> Distance -> Distance
scale s d p = let
    (l,o) = d (p </> s)  
  in (l*s, \p -> o (p </> s))

transpose :: Vector -> Distance -> Distance
transpose v d p = let
    (l,o) = d (p <-> v)  
  in (l, \p -> o (p <-> v))

rotate :: Quaternion -> Distance -> Distance
rotate q d p = let
    let t = apply (inverse q) p
    (l,o) = d t
  in (l, \p -> o t)
