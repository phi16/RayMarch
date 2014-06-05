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
  return $ t<*>(1-r) <+> u<*>r

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
