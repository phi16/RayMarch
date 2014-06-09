module RayMarch.Default.Sample where

import RayMarch.Types
import RayMarch.Primitive
import RayMarch.Distance
import RayMarch.Quaternion
import RayMarch.Object
import RayMarch.Operate
import RayMarch.Default.Object

light :: Point
light = Vector (0,5,5)

testSphere :: Distance s
testSphere = (sp <|> bx) <|> ground <|> bound where
  sp = transpose (Vector (5,0.5,1)) $ sphere 1 $ metalic $ Color (1,0.5,0)
  bx = transpose (Vector (5,-0.5,1)) $ 
       rotate (xRotate (rad 40)`prod`yRotate (rad 60)) $ 
       box (Vector (0.7,0.7,0.7)) $ metalic $ Color (0,0,1)
  ground = plane (Vector (0,0,1)) 0 $ metalic $ Color (0,1,0)
  bound = invert $ (sphere 30) $ emission $ Color (0.5,1,1)
  ao = ambientOcclusion 2.0 0.1
  metalic c = (alpha 0.5 mirror`lighten`phong (0.0,1.0,0.5) 5.0 light c)`darken`ao

testView :: View
testView = View {
  position = Vector (0,0,1),
  direction = defQ,
  fov = 60.0,
  ratio = 0.75
}
