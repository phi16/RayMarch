module RayMarch.Run where

import Data.Maybe
import Control.Monad.Trans.State
import Codec.Picture hiding (Pixel)
import RayMarch.Types
import RayMarch.March
import RayMarch.Quaternion

import Debug.Trace (trace)

runMarcher :: Config -> World s -> IO ()
runMarcher cfg wld = savePngImage (fileName cfg) $ ImageRGB8 img where
  world = wld {viewPoint = position $ view cfg}
  w = width cfg
  h = w * ratio (view cfg) 
  rt = 0.5*h/w
  img = flip ((flip generateImage) (floor w)) (floor h) $ \x y ->let 
      px = (fromIntegral x/w-0.5,-(fromIntegral y/w-rt))
      Color (r,g,b) = fst $ runState (getColor cfg px) $ world
      toP8 t = fromIntegral $ max 0 $ min 255 $ floor $ t*256
    in PixelRGB8 (toP8 r) (toP8 g) (toP8 b)

getColor :: Config -> Pixel -> March s Color
getColor cfg px@(x,y) = do
  w <- get
  let vw = view cfg
      p = position vw
      v = norm $ flip apply (Vector (1,x,y)) $ direction vw
  cu <- (if abs x < 0.001 then trace (show (100-(y*4/3+0.5)*100) ++ "%") else id) $ advance Nothing p v
  return $ effector w p cfg px cu
