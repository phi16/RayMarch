module RayMarch.Quaternion (
  Quaternion,
  apply,prod,
  defQ,
  xRotate,yRotate,zRotate,
  inverse) where

import System.Random
import RayMarch.Types hiding (norm)

add :: Quaternion -> Quaternion -> Quaternion
add (v,t) (w,u) = (v<+>w, t + u)

prod :: Quaternion -> Quaternion -> Quaternion
prod (v,t) (w,u) = (w<*>t <+> v<*>u <+> v`cross`w,t*u-v`dot`w)

neg :: Quaternion -> Quaternion
neg (v,t) = (inv v, -t)

conj :: Quaternion -> Quaternion
conj (v,t) = (inv v,t)

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
inverse q = conj q`scale`recip (lenSq q)

apply :: Quaternion -> Vector -> Vector
apply q u = let (v,_) = (conj q)`prod`(u,0)`prod`q in v

defQ :: Quaternion
defQ = (zero,1)

xRotate :: Float -> Quaternion
xRotate t = (Vector (sin (t/2),0,0),cos (t/2))

yRotate :: Float -> Quaternion
yRotate t = (Vector (0,sin (t/2),0),cos (t/2))

zRotate :: Float -> Quaternion
zRotate t = (Vector (0,0,sin (t/2)),cos (t/2))

instance Random (Vector, Float) where
  randomR _ _ = error "Unimplemented"
  random g = let
      (u1,s1) = random g
      (u2,s2) = random s1
      (u3,s3) = random s2
      a = sqrt $ 1-u1
      b = sqrt u1
      p = 2*pi*u2
      q = 2*pi*u3
    in ((Vector (a*sin p,a*cos p,b*sin q),b*cos q),s3)
