module Main (main) where

import System.Random
import RayMarch.Types
import RayMarch.Run
import RayMarch.Operate
import RayMarch.Distance
import RayMarch.Object
import RayMarch.Quaternion
import RayMarch.Primitive
import RayMarch.Field
import RayMarch.Util
import RayMarch.Default.World
import RayMarch.Default.Advancer
import RayMarch.Default.Effector
import RayMarch.Default.Object
import RayMarch.Default.Lens
import RayMarch.Default.Sample

main :: IO ()
main = runMarcher config world where
  width = 600
  config = Config "out.png" view width
  world = defaultWorld {
    distancer = dists,
    advancer = simpleAdvancer,
    effector = noEffect, 
    backGround = black,
    reflectLimit = 4
  }
  view = View {
    position = Vector (-1.2,0,0.5),
    direction = yRotate $ rad (-2),
    lens = fisheyeLens 90,
    ratio = 0.75
  }
  light = Vector (0,-5,10)
  bxSz = Vector (0.2,0.2,0.2)
  metalic = fogV $ base`darken`shadow where
    fogV = fog 0.05 black light black
    base = alpha 0.5 mirror`lighten`diff`lighten`spec
    diff = rainbow 0.4 light (halfLambert light)
    spec = cookTorrance 0.3 10.0 light
    shadow = blend 0.5 (emission white) $ ao`darken`soft
    ao = ambientOcclusion 3.0 0.1
    soft = blend 0.5 (emission white) $ softShadow 4.0 light
  ground = plane (Vector (0,0,1)) 0 $ metalic
  bound = invert $ sphere 100 $ emission black
  gnd = ground
  g = mkStdGen 10
  count = 50
  (vs,vg) = takeRandom count g
  (qs,qg) = takeRandom count vg
  rs = zip vs qs
  bxs = fusionFor rs $ \(Vector (x,y,z),q) -> transpose (Vector (0.5+x*5,y*4-2,z*1.5)) $ rotate q $ box bxSz metalic 
  dists = bound <|> gnd <|> bxs
