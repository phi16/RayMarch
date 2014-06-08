module RayMarch.March where

import Control.Applicative
import Control.Monad.Trans.State
import RayMarch.Types

eps :: Float
eps = 0.0001

distance :: Point -> March (Float, Object)
distance p = do
  l <- distancer <$> get
  return $ l p

advance :: Point -> Vector -> March Color
advance p v = do
  w <- get
  let a = advanceCount w
  if a <= advanceLimit w
    then do
      put $ w {advanceCount = a+1}
      c <- advancer w p v
      put $ w {advanceCount = a}
      return c
    else backGroundColor

reflect :: Point -> Vector -> March Color
reflect p v = do
  w <- get
  let a = advanceCount w
      r = reflectCount w
  if r <= reflectLimit w
    then do
      put $ w {advanceCount = 0, reflectCount = r+1}
      c <- advancer w p v
      put $ w {advanceCount = a, reflectCount = r}
      return c
    else backGroundColor

maxReflect :: Int -> March a -> March (Maybe a)
maxReflect x a = do
  w <- get
  let r = reflectCount w
      u = reflectLimit w
  if r <= u
    then do
      put $ w {reflectCount = r+1, reflectLimit = x}
      t <- a
      put $ w {reflectCount = r, reflectLimit = u}
      return $ Just t
    else return Nothing
