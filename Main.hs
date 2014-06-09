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
  width = 200
  config = Config "out.png" testView width
  world = defaultWorld {
    distancer = testSphere,
    advancer = antiAliasedAdvancer width,
    effector = noEffect, 
    backGround = black
  } 
