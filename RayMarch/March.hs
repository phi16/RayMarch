module RayMarch.March where

import Control.Applicative
import Control.Monad.Trans.State
import RayMarch.Types

distance :: Point -> March s (Float, Object s)
distance p = do
  l <- distancer <$> get
  return $ l p

advance :: Maybe s -> Point -> Vector -> March s Color
advance s p v = do
  w <- get
  let a = advanceCount w
  if a <= advanceLimit w
    then do
      put $ w {advanceCount = a+1}
      c <- advancer w s p v
      put w
      return c
    else if reflectCount w /= reflectLimit w
      then do
        (_,o) <- distance p
        o p v
      else backGroundColor

reflect :: Point -> Vector -> March s Color
reflect p v = do
  w <- get
  let r = reflectCount w
      n = w {advanceCount = 0, reflectCount = r+1 {- viewPoint = p -} }
  if r <= reflectLimit w
    then do
      put n
      c <- advancer n Nothing p v
      put w
      return c
    else backGroundColor

maxReflect :: Int -> March s a -> March s (Maybe a)
maxReflect x a = do
  w <- get
  let r = reflectCount w
      u = reflectLimit w
  if r <= u
    then do
      put $ w {reflectCount = r+1, reflectLimit = x}
      t <- a
      put w
      return $ Just t
    else return Nothing
