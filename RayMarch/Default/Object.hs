module RayMarch.Default.Object where

import Control.Monad
import Control.Applicative hiding ((<*>))
import Data.Maybe
import RayMarch.Types
import RayMarch.Operate
import RayMarch.March
import RayMarch.Object

type Vector4 = (Vector,Vector,Vector,Vector)

rayLNRV :: Point -> Vector -> Point -> March Vector4
rayLNRV p v l = do
  let lu = norm $ p<->l
      vu = norm v
  nu <- normal p
  let ru = norm $ lu`reflectOn`nu
  return (lu,nu,ru,vu)

normalDisp :: Object
normalDisp p v = do
  Vector (r,g,b) <- normal p
  return $ Color (r/2+0.5,g/2+0.5,b/2+0.5)

lambert :: Point -> Color -> Object
lambert l c p v = do
  (lu,nu,_,_) <- rayLNRV p v l
  return $ c<*>(max 0 $ lu`dot`nu)

halfLambert :: Point -> Color -> Object
halfLambert l c p v = do
  (lu,nu,_,_) <- rayLNRV p v l
  return $ c<*>(lu`dot`nu/2+0.5)

phongSpecular :: Float -> Point -> Object
phongSpecular b l p v = do
  (_,_,ru,vu) <- rayLNRV p v l
  return $ gray $ (max 0 $ ru`dot`vu)**b

phong :: (Float, Float, Float) -> Float -> Point -> Color -> Object
phong (ka,kd,ks) b l c = ambient`lighten`diffuse`lighten`specular where
  ambient  = alpha ka $ emission c
  diffuse  = alpha kd $ lambert l c
  specular = alpha ks $ phongSpecular b l
   
blinnPhong :: Float -> Point -> Color -> Object
blinnPhong b l c p v = do
  (lu,nu,ru,vu) <- rayLNRV p v l
  let hu = norm $ lu <+> vu
      d = c<*>(lu`dot`nu/2+0.5)
      s = gray $ (max 0 $ nu`dot`hu)**b
  return $ d<+>s

-- oren-nayer
-- minnaert function

ambientOcclusion :: Float -> Float -> Object
ambientOcclusion k d p v = do
  n <- normal p
  aos <-sequence $ flip map [1..5] $ \i -> do
    (dist,_) <- distance $ p<->n<*>i<*>d
    return $ 2**(-i)*min 1 (i*d-dist)
  return $ gray $ clamp (1-k*sum aos) (0,1)

coloredAmbient :: Float -> Float -> Color -> Object
coloredAmbient k d c p v = do
  n <- normal p
  aos <-sequence $ flip map [1..5] $ \i -> do
    (dist,o) <- distance $ p<->n<*>i<*>d
    let pi = p<+>v<*>dist
        vi = norm $ pi<->p
    e <- maxReflect 1 $ o pi vi 
    return $ case e of
      Just t -> t <*> (2**(-i)*min 1 (i*d-dist))
      Nothing -> c <*> (2**(-i))
  return $ (<*>k) $ foldl1 (<+>) aos

-- soft shadow 
-- subsurface scattering

mirror :: Object
mirror p v = do
  n <- normal p
  let r = v`reflectOn`n
  t <- reflect (p<+>r<*>0.1) r
  return $ fromMaybe black t

-- glass :: Float -> Object
-- glass f p v = do

-- beckmann distribution
-- heidrich-seidel
-- ward distribution
-- cook-torrance

emission :: Color -> Object
emission c p v = return c
