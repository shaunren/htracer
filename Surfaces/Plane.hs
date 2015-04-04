module Surfaces.Plane where

import Math
import Scene

-- Infinite plane
-- given by eqn (x-p) . n = 0
data Plane = Plane {   point       :: Point
                     , normal      :: Normal
                     , material    :: Material
                     }

instance Surface Plane where
  intersect (Plane p n _) (Ray o d) = if abs a > epsilon && t>epsilon then Just t else Nothing
    where a = (d |.| n)
          t = ((p-o) |.| n)/a

  getNormal (Plane _ n _) _ = n

  getMaterial (Plane _ _ m) _ = m
  
