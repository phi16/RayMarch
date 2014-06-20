module Main (main) where

import RayMarch.Types
import RayMarch.Run
import RayMarch.Operate
import RayMarch.Distance
import RayMarch.Object
import RayMarch.Primitive
import RayMarch.Quaternion
import RayMarch.Default.World
import RayMarch.Default.Advancer
import RayMarch.Default.Effector
import RayMarch.Default.Object
import RayMarch.Default.Lens

main :: IO ()
main = runMarcher config world where
  width = 600
  config = Config "out.png" view width
  world = defaultWorld {
    distancer = dists,
    advancer = simpleAdvancer, -- antiAliasedAdvancer width,
    effector = noEffect, 
    backGround = black,
    reflectLimit = 3
  }
  view = View {
    position = Vector (-1.2,0,0.5),
    direction = yRotate $ rad (-5),
    lens = fisheyeLens 90,
    ratio = 0.75
  }
  light = Vector (10,5,10)
  bxSz = Vector (0.2,0.2,0.2)
  eFog = fog 0.03 (Color (0.3,0.7,1.0)) light (Color (1.2,0.8,0.5))
  sFog = fog 0.03 (Color (0.3,0.7,1.0)) (inv light) (Color (1.2,0.8,0.5))
  metalic c = eFog $ (alpha 0.5 mirror`lighten`phong (0.0,1.0,0.5) 5.0 light c)`darken`(alpha 0.5 $ ambientOcclusion 3.0 0.1`lighten`(blend 1.0 (emission white) $ softShadow 8.0 light))`lighten`coloredAmbient 2.0 0.1 c
  ground = plane (Vector (0,0,1)) 0 $ metalic $ Color (0.8,0.4,0.0)
  cur = transpose (Vector (0.2,-0.3,0.2)) $ rotate (zRotate $ rad 60) $ box bxSz $ metalic $ Color (1.0,1.0,1.0)
  sph = transpose (Vector (21,5,4)) $ rotate (zRotate (rad 30)`prod`yRotate (rad 30)) $ box (bxSz<*>10) $ metalic $ Color (1.5,0.7,0) 
  bound = invert $ sphere 100 $ sFog $ emission black
  bump = scale 0.25 $ push (thickness 0.08 $ h 7) 0.16 undefined where
    fi = (2**) . fromIntegral
    h n (x,y) = hilbert n (x+fi (n-1),y+fi (n-1))
  gnd = ground <\> rotate (zRotate $ rad 30) bump
  obj = cur <|> sph
  dists = bound <|> gnd <|> obj

eLine :: (Float,Float) -> Float
eLine (x,y) = (0.5-x)`max`abs (0.5-y)
  
hilbert :: Int -> (Float,Float) -> Float
hilbert 0 (p,q) = abs (p-0.5)`max`abs (q-0.5)
hilbert n (p,q) = let
    h = hilbert $ n-1
    u = 2 ** (fromIntegral n-1)
  in case (p <= u, q <= u) of
    (True,True) ->   h (u-q,p)`min`eLine (q-u+1,p)
    (True,False) ->  h (p,q-u)`min`eLine (u+1-q,p)`min`eLine (p-u+1,q-u)
    (False,False) -> h (p-u,q-u)`min`eLine (u+1-q,2*u-p)`min`eLine (u+1-p,q-u)
    (False,True) ->  h (q,2*u-p)`min`eLine (q-u+1,2*u-p)

