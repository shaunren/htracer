module AABB where

import Math

-- Axis-Aligned Bounding Box (AABB)
data AABB = AABB { minPoint :: Point, maxPoint :: Point } deriving (Eq, Show)

inside :: Point -> AABB -> Bool
inside pt (AABB { minPoint = minP, maxPoint = maxP }) =
  minP `allCoordsLE` pt && pt `allCoordsLE` maxP

intersect :: AABB -> AABB -> Bool
intersect (AABB { minPoint = minP, maxPoint = maxP }) (AABB { minPoint = minP', maxPoint = maxP' })  =
  minP `allCoordsLE` maxP' && minP' `allCoordsLE` maxP

-- |Clip the second AABB to the first AABB.
clip :: AABB -> AABB -> Maybe AABB
clip (AABB { minPoint = minE, maxPoint = maxE }) (AABB { minPoint = minP, maxPoint = maxP })
  | minP `allCoordsLE` maxE && minE `allCoordsLE` maxP =
    Just $ AABB { minPoint = minP', maxPoint = maxP' }
  | otherwise = Nothing
  where [minx', miny', minz'] = map (uncurry max) $ vzip minE minP
        [maxx', maxy', maxz'] = map (uncurry min) $ vzip maxE maxP
        minP' = Vec3 minx' miny' minz'
        maxP' = Vec3 maxx' maxy' maxz'
