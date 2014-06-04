module RayMarch.Object where

import RayMarch.Types

modify :: Distance -> Object -> Distance
modify f o p = (fst $ f p, o)

-- diffuse
-- specular
-- ambient occulusion
-- shadow 
-- subsurface scattering

-- Light?

emission :: Color -> Object
emission p v c = return c
