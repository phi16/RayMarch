module Main (main) where

import RayMarch.Types
import RayMarch.Run
import RayMarch.Operate
import RayMarch.Distance
import RayMarch.Object
import RayMarch.Primitive
import RayMarch.Quaternion
import RayMarch.Field
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
  light = Vector (0,-5,10)
  bxSz = Vector (0.2,0.2,0.2)
  metalic c = (alpha 0.5 mirror`lighten`(rainbow 0.5 light $ halfLambert light)`lighten`heidrichSeidel 5.0 5.0 rel light)`darken`(ambientOcclusion 3.0 0.1`darken`(blend 0.5 (emission white) $ softShadow 4.0 light))
  ground = plane (Vector (0,0,1)) 0 $ metalic $ Color (0.8,0.4,0.0)
  bound = invert $ sphere 100 $ emission black
  gnd = ground
  spr = transpose (Vector (0,0,0.3)) $ push (circle 0.4`sub`circle 0.3) 0.05 $ metalic white
  pal = push (circle 0.1) 0.8 $ metalic white
  spe = transpose (Vector (0,0,0.8)) $ sphere 0.2 $ metalic white
  obj = transpose (Vector (0,0,0.3)) $ rotate (xRotate (rad 10)`prod`yRotate (rad (-20))) $ spr <|> pal <|> spe
  bxs = repeatX 4 $ repeatY 4 $ transpose (Vector (-1,-1,0.2)) $ 
    (sphere 0.2 $ metalic white) <|> 
    (transpose (Vector (2,0,0)) $ box (Vector (0.2,0.2,0.2)) $ metalic white) <|> 
    (transpose (Vector (2,2,0)) $ sphere 0.2 $ metalic white) <|> 
    (transpose (Vector (0,2,0)) $ box (Vector (0.2,0.2,0.2)) $ metalic white)
  dists = bound <|> gnd <|> obj <|> bxs
  rel = (xRotate (rad 10)`prod`yRotate (rad (-20)))`apply`Vector (0,1,0)




