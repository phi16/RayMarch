module RayMarch.Field where

import RayMarch.Types

sub :: Field -> Field -> Field
sub f g p = f p`max`(-g p)