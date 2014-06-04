module RayMarch.Quaternion (
  Quaternion,
  apply,
  inverse) where

import RayMarch.Types hiding (norm)

type Quaternion = (Vector,Float)

add :: Quaternion -> Quaternion -> Quaternion
add (v,t) (w,u) = (v<+>w, t + u)

prod :: Quaternion -> Quaternion -> Quaternion
prod (v,t) (w,u) = (t*u-v`dot`w, w<*>t + v<*>u + v`cross`w)

neg :: Quaterion -> Quaternion
neg (v,t) = (inv v, -t)

conj :: Quaternion -> Quaternion
conj (v,t) = (-v,t)

scale :: Quaternion -> Float -> Quaternion
scale (v,t) r = (v<*>r, t * r)

expt :: Quaternion -> Quaternion
expt (v,t) = let n = len v in (v</>n<*>sin n, cos n)`scale`exp t

logn :: Quaternion -> Quaternion
logn q@(v,t) = let 
    n = len v
    l = norm q
  in (v</>n<*>acos(t/l),log l)

power :: Quaternion -> Float -> Quaternion
power q a = expt (logn q`scale`a)

lenSq :: Quaternion -> Float
lenSq (v,t) = let l = len v in l*l+t*t

norm :: Quaternion -> Float
norm = sqrt . lenSq

inverse :: Quaternion -> Quaternion
inverse q = conj q`prod`recip (lenSq q)

apply :: Quaternion -> Vector -> Vector
apply q u = let (v,_) = (conj q)`prod`(u,0)`prod`q in v
