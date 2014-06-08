module RayMarch.Default.Advancer where

import Control.Applicative hiding ((<*>))
import RayMarch.Types
import RayMarch.Operate
import RayMarch.March

simpleAdvancer :: Point -> Vector -> March Color
simpleAdvancer p v = do
  (d,o) <- distance p
  if d < eps
    then o p v
    else advance (p<+>v<*>d) v

antiAliasedAdvancer :: Float -> Point -> Vector -> March Color
antiAliasedAdvancer w p v = do
  u <- getViewPoint
  let l = len $ u <-> p
      e = 1.0 * l / w
  (d,o) <- distance p
  if d+e < eps
    then o p v
    else do
      c <- advance (p<+>v<*>(d+e)) v
      if d < e
        then do
          (g,_) <- distance $ p<->v<*>(d+e)
          (f,_) <- distance $ p<+>v<*>(d+e)
          if d < g && d < f
            then do
              let r = (e-d) / (2*e)
              q <- o p v
              return $ lerp r c q
            else return c
        else return c
