module RayMarch.Util where

import System.Random

takeRandom :: (Random a, RandomGen g) => Int -> g -> ([a], g)
takeRandom c g = f c g (,) where
  f 0 g p = p [] g
  f n g p = let
      (v,vs) = random g
    in f (n-1) vs $ \xs ge -> p (v:xs) ge
