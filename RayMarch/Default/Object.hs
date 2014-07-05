module RayMarch.Default.Object where

import Control.Monad
import Control.Applicative hiding ((<*>))
import Control.Monad.Trans.State
import RayMarch.Types
import RayMarch.Operate
import RayMarch.March
import RayMarch.Object

type Vector4 = (Vector,Vector,Vector,Vector)

rayLNRV :: Point -> Vector -> Point -> March s Vector4
rayLNRV p v l = do
  let lu = norm $ p<->l
  nu <- normal p
  e <- getViewPoint
  let ru = norm $ lu`reflectOn`nu
      vu = norm $ e<->p
  return (lu,nu,ru,vu)

normalDisp :: Object s
normalDisp p v = do
  Vector (r,g,b) <- normal p
  return $ Color (r,g,b)</>2<+>gray 0.5

lambert :: Point -> Color -> Object s
lambert l c p v = do
  (lu,nu,_,_) <- rayLNRV p v l
  return $ c<*>(lu`dotP`nu)

halfLambert :: Point -> Color -> Object s
halfLambert l c p v = do
  (lu,nu,_,_) <- rayLNRV p v l
  return $ c<*>(lu`dot`nu/2+0.5)

phongSpecular :: Float -> Point -> Object s
phongSpecular b l p v = do
  (_,_,ru,vu) <- rayLNRV p v l
  return $ gray $ (ru`dotP`vu)**b

phong :: (Float, Float, Float) -> Float -> Point -> Color -> Object s
phong (ka,kd,ks) b l c = ambient`lighten`diffuse`lighten`specular where
  ambient  = alpha ka $ emission c
  diffuse  = alpha kd $ lambert l c
  specular = alpha ks $ phongSpecular b l
   
blinnPhong :: (Float, Float, Float) -> Float -> Point -> Color -> Object s
blinnPhong (ka,kd,ks) b l c p v = do
  (lu,nu,ru,vu) <- rayLNRV p v l
  let hu = norm $ lu <-> vu
      d = c<*>(lu`dot`nu/2+0.5)
      s = gray $ (nu`dotP`hu)**b
  return $ c<*>ka<+>d<*>kd<+>s<*>ks

-- oren-nayer
-- minnaert function

ambientOcclusion :: Float -> Float -> Object s
ambientOcclusion k d p v = do
  n <- normal p
  aos <- sequence $ flip map [1..5] $ \i -> do
    (dist,_) <- distance $ p<->n<*>i<*>d
    return $ 2**(-i)*min 1 (i*d-dist)
  return $ gray $ clamp (1-k*sum aos) (0,1)

coloredAmbient :: Float -> Color -> Object s
coloredAmbient d c p v = do
  n <- normal p
  aos <-sequence $ flip map [1..5] $ \i -> do
    (dist,o) <- distance $ p<->n<*>i<*>d
    let pi = p<+>v<*>dist
        vi = norm $ pi<->p
        b = (2**) $ if i == 5 then -4 else -i
    e <- maxReflect 1 $ o pi vi 
    return $ case e of
      Just t -> t <*> (b*clamp (i*d-dist) (0,1))
      Nothing -> c <*> b
  return $ foldl1 (<+>) aos

softShadow :: Float -> Vector -> Object s
softShadow k l p v = do
  (lu,nu,_,_) <- rayLNRV p v l
  w <- get
  let r = advanceLimit w
      u = len $ p<->l
      march x p v l
        | x >= r = return 1.0
        | otherwise = do
          (d,_) <- distance p
          if d+l >= u
            then return 1.0
            else if d < eps
              then return 0.0
              else do
                m <- march (x+1) (p<+>v<*>d) v (l+d)
                return $ min m ((k*d/l)^2)
  let iu = inv lu
      pu = p<+>iu<*>delta
  t <- march 0 pu iu delta 
  return $ gray t

-- subsurface scattering

mirror :: Object s
mirror p v = do
  n <- normal p
  let r = v`reflectOn`n
  reflect (p<+>r<*>delta) r

-- refraction

beckmann :: Float -> Vector -> Object s
beckmann m l p v = do
  (lu,nu,_,vu) <- rayLNRV p v l
  let hu = norm $ lu <-> vu
      a = nu`dotP`hu
      nume = exp $ (a^2-1)/(m*a)^2
      deno = pi * m^2 * a**4
  return $ gray $ max 0 $ nume/deno

heidrichSeidel :: Float -> Float -> Vector -> Vector -> Object s
heidrichSeidel n m d l p v = do
  (lu,nu,_,vu) <- rayLNRV p v l
  let du = norm d
      c p q = p`dot`q
      s p q = p`crossF`q
      tu = norm $ nu`cross`(du`cross`nu)
      k = c lu tu*c vu tu + s lu tu*s vu tu
  r <- phongSpecular n l p v
  return $ r<*>(max 0 k**m)

cookTorrance :: Float -> Float -> Vector -> Object s
cookTorrance m n l p v = do
  (lu,nu,_,vu) <- rayLNRV p v l
  let eu = inv vu
      hu = norm $ lu <+> eu
      en = eu`dotP`nu
  ds <- beckmann m l p v
  let fs = r0 + (1-r0)*(1-en)^5 where 
        r0 = ((1-n)/(1+n))^2
      gs = min 1 (p*2*(hu`dotP`nu)/(eu`dotP`hu)) where
        p = en`min`(lu`dotP`nu)
  return $ ds<*>(max 0 $ fs*gs / en)

fog :: Float -> Color -> Vector -> Color -> Object s -> Object s
fog x c s d o p v = do
  e <- getViewPoint
  let l = len $ e<->p
      m = exp $ -l * x
      i = inv v`dotP`norm s
  u <- o p v
  return $ lerp m (lerp i c d) u

rainbow :: Float -> Vector -> (Color -> Object s) -> Object s
rainbow f l o p v = do
  n <- normal p
  let s = (v`crossF`n + (norm l)`dot`n) + f
      c = Color (sin(s*pi),sin(s*pi+pi*2/3),sin(s*pi-pi*2/3))
  o ((c<+>white)</>2) p v

emission :: Color -> Object s
emission c p v = return c
