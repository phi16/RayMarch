module Main (main) where

import RayMarch.Types
import RayMarch.Run
import RayMarch.Operate
import RayMarch.Default.Sample
import RayMarch.Default.World
import RayMarch.Default.Advancer
import RayMarch.Default.Effector

main :: IO ()
main = runMarcher config world where
  config = Config "out.png" testView 600 black
  world = defaultWorld {
    distancer = testSphere,
    advancer = simpleAdvancer,
    effector = noEffect
  } 
