module Multisampler where

import Math
import Control.Applicative
import Control.Arrow

-- multisampling transformer
-- vright -> vup -> pt -> [pts]
type Multisampler = Vec3 -> Vec3 -> Point -> [Point]

-- supersample point offsets
msaa :: [(Scalar,Scalar)] -> Multisampler
msaa offsets vright vup p = [p + (x|*vright) + (y|*vup) | (x,y) <- offsets]

-- standard MSAA patterns
stdMsaa :: [(Scalar,Scalar)] -> Multisampler
stdMsaa = msaa . (((/16) *** (/16)) <$>)

noaa, msaa2, msaa4, msaa8, msaa16 :: Multisampler
noaa _ _ = pure
msaa2  = stdMsaa [(-4,4),(4,-4)]
msaa4  = stdMsaa [(-2,6),(6,2),(-6,-2),(2,-6)]
msaa8  = stdMsaa [(1,3),(-1,-3),(5,-1),(-3,5),(-5,-5),(-7,1),(3,-7),(7,7)]
msaa16 = stdMsaa [(1,-1),(-1,3),(-3,-2),(4,1),(-5,2),(2,-5),(5,-3),(3,5),
                  (-2,-6),(0,7),(-4,6),(-6,-4),(-8,0),(7,4),(6,-7),(-7,8)]
