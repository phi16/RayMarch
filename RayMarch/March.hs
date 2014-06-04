module RayMarch.March where

import Control.Applicative
import RayMarch.Types

eps :: Float
eps = 0.0001

distance :: Point -> March (Float, Object)
distance p = do
  l <- distancer <$> world
  return $ l p

advance :: Point -> Vector -> March (Maybe Color)
advance p v = do
  w <- world
  let a = advanceCount w
  if a <= advanceLimit w
    then do
      setWorld $ w {advanceCount = a+1}
      c <- advancer w p v
      setWorld $ w {advanceCount = a}
      return $ Just c
    else return Nothing

reflect :: Point -> Vector -> March (Maybe Color)
reflect p v = do
  w <- world
  let a = advanceCount w
      r = reflectCount w
  if r <= reflectLimit w
    then do
      setWorld $ w {advanceCount = 0, reflectCount = r+1}
      c <- advancer w p v
      setWorld $ w {advanceCount = a, reflectCount = r}
      return $ Just c
    else return Nothing

normal :: Point -> March Vector
normal p = do
  (x1,_) <- distance $ p <+> Vector (eps,0,0)
  (x2,_) <- distance $ p <-> Vector (eps,0,0)
  (y1,_) <- distance $ p <+> Vector (eps,0,0)
  (y2,_) <- distance $ p <-> Vector (eps,0,0)
  (z1,_) <- distance $ p <+> Vector (eps,0,0)
  (z2,_) <- distance $ p <-> Vector (eps,0,0)
  return $ inv $ norm $ Vector (x1-x2,y1-y2,z1-z2)
