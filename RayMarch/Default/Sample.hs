module RayMarch.Default.Sample (
  testSphere,
  testView,
  gasket,
  sponge) where

import RayMarch.Types
import RayMarch.Primitive
import RayMarch.Distance
import RayMarch.Quaternion
import RayMarch.Object
import RayMarch.Operate
import RayMarch.Default.Lens
import RayMarch.Default.Object

light :: Point
light = Vector (0,5,5)

testSphere :: Distance s
testSphere =(sp <|> bx) <|> ground <|> bound where
  eFog = fog 0.03 (Color (0.3,0.7,1.0)) light (Color (1.2,0.8,0.5))
  sFog = fog 0.03 (Color (0.3,0.7,1.0)) (inv light) (Color (1.2,0.8,0.5))
  sp = transpose (Vector (5,0.5,1)) $ sphere 1 $ metalic $ Color (1,0.5,0)
  bx = transpose (Vector (5,-0.5,1)) $ 
       rotate (xRotate (rad 40)`prod`yRotate (rad 60)) $ 
       box (Vector (0.7,0.7,0.7)) $ metalic $ Color (0,0,1)
  gsk = transpose (Vector (5,-0.5,1)) $ gasket $ metalic $ Color (0,0,1)
  spg = transpose (Vector (5,-0.5,1)) $ 
        rotate (xRotate (rad 40)`prod`yRotate (rad 60)) $
        scale 0.7 $ sponge $ metalic $ Color (0,0,1)
  ground = plane (Vector (0,0,1)) 0 $ metalic $ Color (0,1,0)
  bound = invert $ (sphere 30) $ sFog $ emission $ Color (0.5,1,1)
  metalic c = eFog $ (alpha 0.5 mirror`lighten`lambert light c`lighten`cookTorrance 0.4 3.0 light)`darken`ambientOcclusion 2.0 0.1

testView :: View
testView = View {
  position = Vector (0,0,1),
  direction = defQ,
  lens = defaultLens,
  ratio = 0.75
}

gasket :: Object s -> Distance s
gasket = foldFractal 2 (Vector (1,1,1)) fs es where
  fs = [(-1,-1,0,0),(-1,0,-1,0),(0,-1,-1,0)]
  es = [(-1,-1,-1,-1),(-1,1,1,-1),(1,-1,1,-1),(1,1,-1,-1)]

sponge :: Object s -> Distance s
sponge = areaFractal 3 xs where
  ps = map Vector [
    (-1,-1,-1),(-1,-1,1),(-1,1,-1),(-1,1,1),
    (1,-1,-1),(1,-1,1),(1,1,-1),(1,1,1),
    (0,1,1),(0,-1,-1),(0,-1,1),(0,1,-1),
    (1,0,1),(-1,0,-1),(-1,0,1),(1,0,-1),
    (1,1,0),(-1,-1,0),(-1,1,0),(1,-1,0)]
  xs = for ps $ \v ->
    (transpose (v<*>(2/3)) $ box (Vector (1,1,1) <*> (1/3+delta)) $ undefined, v)
