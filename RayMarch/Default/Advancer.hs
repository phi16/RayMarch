module RayMarch.Default.Advancer where

import Control.Applicative hiding ((<*>))
import RayMarch.Types
import RayMarch.Operate
import RayMarch.March

simpleAdvancer :: Point -> Vector -> March (Maybe Color)
simpleAdvancer p v = do
  (d,o) <- distance p
  if d < eps
    then Just <$> o p v
    else advance (p<+>v<*>d) v

-- antiAlisedAdvancer :: Point -> Vector -> March (Maybe Color)
