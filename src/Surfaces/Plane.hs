module Surfaces.Plane where

import Math
import Scene
import qualified AABB as B

-- Infinite plane
-- given by eqn (x-p) . n = 0
data Plane = Plane {   point       :: Point
                     , normal      :: Normal
                     , material    :: Material
                     }

instance Surface Plane where
  intersect (Plane p n _) (Ray o d)
    | abs a > epsilon && t > epsilon = Just t
    | otherwise                      = Nothing
    where a = (d |.| n)
          t = ((p-o) |.| n)/a

  getNormal (Plane _ n _) _ = n

  getMaterial (Plane _ _ m) _ = m

  -- Compute the intersections of the plane with every edge in the given AABB,
  -- and take minimum and maximum coordinates as the resulting AABB.
  getAABB plane (B.AABB { B.minPoint = (Vec3 xm ym zm) , B.maxPoint = (Vec3 xM yM zM) })
    | null intersections = Nothing
    | otherwise          = Just $
      B.AABB { B.minPoint = (foldl1 (liftV2 min) intersections)
             , B.maxPoint = (foldl1 (liftV2 max) intersections) }

    where
      edges = [((Ray (Vec3 x y z) (basis !! i)), i) |
               x <- [xm,xM], y <- [ym,yM], z <- [zm,zM],
               i <- [0..2],
               [x,y,z] !! i == mins !! i]
      mins = [xm, ym, zm]
      maxs = [xM, yM, zM]
      intersections = map (\(Just (v,_)) -> v) . filter filterInRange . map mapIntersect $ edges
      mapIntersect (r@(Ray o d), i) = intersect plane r >>= \t -> Just (o + d*|t, i)
      filterInRange Nothing                  = False
      filterInRange (Just ((Vec3 x y z), i)) = [x,y,z] !! i <= maxs !! i
