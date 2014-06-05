module RayMarch.Default.Sample where

import RayMarch.Types
import RayMarch.Primitive
import RayMarch.Distance
import RayMarch.Quaternion
import RayMarch.Object
import RayMarch.Default.Object

light :: Point
light = Vector (0,5,5)

testSphere :: Distance
testSphere = sp <|> ground where
  sp = transpose (Vector (5,0,1)) $ sphere 1 $ surface $ Color (1,0.5,0)
  ground = plane (Vector (0,0,1)) 0 $ surface $ Color (0,1,0)
  surface c = halfLambert light c`darken`ambientOcclusion 1.0 0.3

testView :: View
testView = View {
  position = Vector (0,0,1),
  direction = defQ,
  fov = 60.0,
  ratio = 0.75
}
