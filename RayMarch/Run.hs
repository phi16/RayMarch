module RayMarch.Run where

import Data.Maybe
import RayMarch.Types
import RayMarch.March

runMarcher :: Config -> World -> IO ()
runMarcher cfg w = do
  return ()
  -- (c,_) <- runState (getColor cfg px p v) w
  -- TODO

getColor :: Config -> Pixel -> Point -> Vector -> March Color
getColor cfg px p v = do
  w <- world
  cu <- advance p v
  return $ effector w p cfg px $ fromMaybe (backGround cfg) cu
