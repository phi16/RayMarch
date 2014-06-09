module RayMarch.Default.Advancer (
  simpleAdvancer,
  antiAliasedAdvancer) where

import Control.Applicative hiding ((<*>))
import RayMarch.Types
import RayMarch.Operate
import RayMarch.March

simpleAdvancer :: Maybe () -> Point -> Vector -> March () Color
simpleAdvancer s p v = do
  (d,o) <- distance p
  if d < eps
    then o p v
    else advance s (p<+>v<*>d) v

data AAState = A (Dup (Float,Object AAState))
antiAliasedAdvancer :: Float -> Maybe AAState -> Point -> Vector -> March AAState Color
antiAliasedAdvancer w Nothing p v = do
  u <- distance p
  antiAliasedAdvancer w (Just (A (u,u))) p v
antiAliasedAdvancer w (Just (A (s,t))) p v = do
  u <- getViewPoint
  let l = len $ u <-> p
      e = 0.5 * l / w
  (d,o) <- return t -- Current
  if d+e < eps
    then o p v
    else do
      (f,_) <- return s -- Prev
      m@(g,_) <- distance $ p<+>v<*>(d+e) -- Next
      c <- advance (Just $ A (t,m)) (p<+>v<*>(d+e)) v
      if d < e+eps
        then do
          if d < g && d < f
            then do
              let r = (e-d) / (2*e)
              q <- o p v
              return $ lerp r c q
            else return c
        else return c
