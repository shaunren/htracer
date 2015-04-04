module Math where

import Control.DeepSeq

epsilon :: Double
epsilon = 0.00000001

-----------------------------------------------------------------------------------------------------
-- A 3D vector
data Vec3 = Vec3 { v3x :: Double, v3y :: Double,  v3z :: Double } deriving (Eq)

instance Show Vec3 where
  show (Vec3 x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Num Vec3 where
  (Vec3 x y z) + (Vec3 x' y' z') = Vec3 (x+x') (y+y') (z+z')
  (Vec3 x y z) * (Vec3 x' y' z') = Vec3 (x*x') (y*y') (z*z')
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  abs (Vec3 x y z)    = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger n       = Vec3 (fromInteger n) (fromInteger n) (fromInteger n)

instance NFData Vec3 where
  rnf (Vec3 x y z) = rnf x `seq` rnf y `seq` rnf z `seq` ()

-- the zero vector
zerov :: Vec3
zerov = Vec3 0 0 0

-- map f to each coordinate
vmap :: (Double -> Double) -> Vec3 -> Vec3
vmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

-- scalar product
infixl 7 |*
(|*) :: Double -> Vec3 -> Vec3
(|*) a = vmap (a*)

infixl 7 *|
(*|) :: Vec3 -> Double -> Vec3
(*|) = flip (|*)

-- dot product
infixl 7 |.|
(|.|) :: Vec3 -> Vec3 -> Double
(Vec3 x y z) |.| (Vec3 x' y' z') = x*x' + y*y' + z*z'

-- cross product
infixl 7 |*|
(|*|) :: Vec3 -> Vec3 -> Vec3
(Vec3 x y z) |*| (Vec3 x' y' z') = Vec3 (y*z'-y'*z) (x'*z-x*z') (x*y'-x'*y)

-- component-wise exp
infixr 8 |**|
(|**|) :: Vec3 -> Double -> Vec3
(Vec3 x y z) |**| e = Vec3 (x**e) (y**e) (z**e)

-- vector norm
norm :: Vec3 -> Double
norm v = sqrt $ v |.| v

-- distance between two vectors
distance :: Vec3 -> Vec3 -> Double
distance v w = norm $ v-w

-- normalize vector
normalize :: Vec3 -> Vec3
normalize v@(Vec3 x y z)
  | n < epsilon     = zerov
  | otherwise       = Vec3 (x/n) (y/n) (z/n)
  where n = norm v

-- Vec3 with identical entries
idv :: Double -> Vec3
idv x = Vec3 x x x


------------------------------------------------------------------
-- 3D ray
data Ray = Ray { origin :: Vec3, direction :: Vec3 } deriving (Eq, Show)
        
-- compute the smallest real root(s) to ax^2 + bx + c = 0
roots2 :: Double -> Double -> Double -> [Double]
roots2 a b c
  | disc < (-epsilon)  = []
  | abs disc < epsilon = [-b/(2*a)]
  | otherwise          = let sd = sqrt disc in [(-b-sd)/(2*a), (-b+sd)/(2*a)]
  where disc = b*b - 4*a*c

-- reflection of a vector against a unit normal
reflection :: Vec3 -> Vec3 -> Vec3
reflection d n = d - (2*(d |.| n) |* n)

-- types
type Point  = Vec3
type Normal = Vec3
