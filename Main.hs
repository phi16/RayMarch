module Main (main) where

import RayMarch.Types
import RayMarch.Run
import RayMarch.Operate
import RayMarch.Default.World
import RayMarch.Default.Advancer
import RayMarch.Default.Effector
import RayMarch.Default.Object
import RayMarch.Default.Lens
import RayMarch.Default.Sample

main :: IO ()
main = runMarcher config world where
  width = 600
  config = Config "out.png" testView width
  world = defaultWorld {
    distancer = testSphere,
    advancer = simpleAdvancer,
    effector = noEffect, 
    backGround = black
  }
