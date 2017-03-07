-- Sphere
module Surfaces.Sphere where

import Math
import Scene
import qualified AABB as B

-- Sphere
data Sphere = Sphere { center      :: Point
                     , radius      :: Scalar
                     , textureMap  :: UVMap
                     }

instance Surface Sphere where
  intersect (Sphere c r _) (Ray o d) =
    case (filter (>epsilon) $ roots2 1 (2*(v|.|d)) (v|.|v - r*r)) of
      []    -> Nothing
      (t:_) -> Just t
    where v = o-c

  getNormal (Sphere c _ _) p = normalize (p-c)
    
  -- map XYZ coordinates to UV and get material
  getMaterial (Sphere c _ uvmap) p = uvmap u v
    where (Vec3 dx dy dz) = normalize (p-c)
          u = 0.5 + (atan2 dz dx)/(2*pi)
          v = 0.5 - (asin dy)/pi

  getAABB (Sphere c r _) b = B.clip b $ B.AABB { B.minPoint = vmap (\x -> x - r) c, B.maxPoint = vmap (+r) c }
