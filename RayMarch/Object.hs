module RayMarch.Object where

import RayMarch.Types
import RayMarch.Operate

modify :: Distance s -> Object s -> Distance s
modify f o p = (fst $ f p, o)

blackBody :: Object s
blackBody p v = return black

blend :: Float -> Object s -> Object s -> Object s
blend r a b p v = do
  t <- a p v
  u <- b p v
  return $ lerp r t u

lighten :: Object s -> Object s -> Object s
lighten a b p v = do
  t <- a p v
  u <- b p v
  return $ t<+>u

darken :: Object s -> Object s -> Object s
darken a b p v = do
  Color (tr,tg,tb) <- a p v
  Color (ur,ug,ib) <- b p v
  return $ Color (tr*ur,tg*ug,tb*ib)

alpha :: Float -> Object s -> Object s
alpha r o p v = do
  t <- o p v
  return $ t<*>r
