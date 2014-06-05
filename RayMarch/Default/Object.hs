module RayMarch.Default.Object where

import Control.Monad
import Control.Applicative hiding ((<*>))
import RayMarch.Types
import RayMarch.Operate
import RayMarch.March

type Vector4 = (Vector,Vector,Vector,Vector)

rayLNRV :: Point -> Vector -> Point -> March Vector4
rayLNRV p v l = do
  let lu = norm $ p<->l
      vu = norm $ inv v
  nu <- normal p
  let ru = norm $ v`reflectOn`nu
  return (lu,nu,ru,vu)

normalDisp :: Object
normalDisp p v = do
  Vector (r,g,b) <- normal p
  return $ Color (r/2+0.5,g/2+0.5,b/2+0.5)

lambert :: Point -> Color -> Object
lambert l c p v = do
  (lu,nu,_,_) <- rayLNRV p v l
  return $ c<*>lu`dot`nu

halfLambert :: Point -> Color -> Object
halfLambert l c p v = do
  (lu,nu,_,_) <- rayLNRV p v l
  return $ c<*>(lu`dot`nu/2+0.5)

-- blinn-phong
-- phong
-- lambert
-- half-lambert
-- oren-nayer
-- minnaert function

ambientOcclusion :: Float -> Float -> Object
ambientOcclusion k d p v = do
  n <- normal p
  aos <-sequence $ flip map [1..5] $ \i -> do
    (dist,_) <- distance $ p<->n<*>i<*>d
    return $ 2**(-i)*min 1 (i*d-dist)
  return $ gray $ clamp (1-k*sum aos) (0,1)

-- colored AO
-- soft shadow 
-- subsurface scattering
-- reflect
-- refract

-- beckmann distribution
-- heidrich-seidel
-- ward distribution
-- cook-torrance

-- Light? <- argument

emission :: Color -> Object
emission c p v = return c
