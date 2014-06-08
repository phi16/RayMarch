module RayMarch.Object where

import RayMarch.Types
import RayMarch.Operate

modify :: Distance -> Object -> Distance
modify f o p = (fst $ f p, o)

blackBody :: Object
blackBody p v = return black

blend :: Float -> Object -> Object -> Object
blend r a b p v = do
  t <- a p v
  u <- b p v
  return $ lerp r t u

lighten :: Object -> Object -> Object
lighten a b p v = do
  t <- a p v
  u <- b p v
  return $ t<+>u

darken :: Object -> Object -> Object
darken a b p v = do
  Color (tr,tg,tb) <- a p v
  Color (ur,ug,ib) <- b p v
  return $ Color (tr*ur,tg*ug,tb*ib)

alpha :: Float -> Object -> Object
alpha r o p v = do
  t <- o p v
  return $ t<*>r
