module RayMarch.Types where

import Control.Monad.Trans.State
import Control.Applicative hiding ((<*>))

type Float3 = (Float, Float, Float)
newtype Vector = Vector Float3
type Point = Vector
newtype Color = Color Float3
type Quaternion = (Vector, Float)

class Arith a where
  (<->) :: a -> a -> a
  (<*>) :: a -> Float -> a
  (</>) :: a -> Float -> a
  inv :: a -> a
  x <*> y = x </> (recip y)
  x </> y = x <*> (recip y)
  inv v = v <*> (-1)

class Additive a where
  (<+>) :: a -> a -> a

class Arith a => Direction a where
  len :: a -> Float
  unit :: a
  norm :: a -> a
  norm v = if len v == 0 then unit else v </> len v

class Each a where
  each :: (Float -> Float) -> a -> a
  fold :: (Float -> Float -> Float) -> a -> Float

infixl 6 <+>
infixl 6 <->
infixl 7 <*>
infixl 7 </>

instance Arith Vector where
  (Vector (a,b,c)) <-> (Vector (d,e,f)) = Vector (a-d,b-e,c-f)
  (Vector (a,b,c)) <*> r = Vector (a*r,b*r,c*r)

instance Additive Vector where
  (Vector (a,b,c)) <+> (Vector (d,e,f)) = Vector (a+d,b+e,c+f)

instance Direction Vector where
  len (Vector (a,b,c)) = sqrt (a*a+b*b+c*c)
  unit = Vector (1,0,0)

instance Each Vector where
  each f (Vector (a,b,c)) = Vector (f a,f b,f c)
  fold f (Vector (a,b,c)) = f (f a b) c

dot :: Vector -> Vector -> Float
dot (Vector (a,b,c)) (Vector (d,e,f)) = a*d+b*e+c*f

cross :: Vector -> Vector -> Vector
cross (Vector (a,b,c)) (Vector (d,e,f)) = Vector (b*f-c*e,c*d-a*f,a*e-b*d)

zero :: Vector
zero = Vector (0,0,0)

instance Arith Color where
  (Color (a,b,c)) <-> (Color (d,e,f)) = Color (a-d,b-e,c-f)
  (Color (a,b,c)) <*> r = Color (a*r,b*r,c*r)

instance Additive Color where
  (Color (a,b,c)) <+> (Color (d,e,f)) = Color (a+d,b+e,c+f)

instance Each Color where
  each f (Color (a,b,c)) = Color (f a,f b,f c)
  fold f (Color (a,b,c)) = f (f a b) c

type Pixel = (Float,Float)

data View = View {
  position :: Point,
  direction :: Quaternion,
  fov :: Float,
  ratio :: Float
}

data Config = Config {
  fileName :: FilePath,
  view :: View,
  width :: Float,
  backGround :: Color
}

data World = World {
  distancer :: Distance,
  advancer :: Point -> Vector -> March (Maybe Color),
  effector :: Point -> Config -> Pixel -> Color -> Color,
  advanceCount :: Int,
  advanceLimit :: Int,
  reflectCount :: Int,
  reflectLimit :: Int
}

type March = State World

type Distance = Point -> (Float, Object)
type Object = Point -> Vector -> March Color

getAdvanceLimit :: March Int
getAdvanceLimit = advanceLimit <$> get