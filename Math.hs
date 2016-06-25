module Math where

import Control.DeepSeq
--import Data.Vector.Unboxed

-- scalar type
type Scalar = Double

epsilon :: Scalar
epsilon = 0.000001

-----------------------------------------------------------------------------------------------------
-- A 3D vector
data Vec3 = Vec3 Scalar Scalar Scalar deriving (Eq)

instance Show Vec3 where
  show (Vec3 x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Num Vec3 where
  (Vec3 x y z) + (Vec3 x' y' z') = Vec3 (x+x') (y+y') (z+z')
  (Vec3 x y z) * (Vec3 x' y' z') = Vec3 (x*x') (y*y') (z*z')
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  abs (Vec3 x y z)    = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger n       = let x = fromInteger n in Vec3 x x x

instance NFData Vec3 where
  rnf (Vec3 x y z) = rnf x `seq` rnf y `seq` rnf z `seq` ()

-- the zero vector
zerov :: Vec3
zerov = Vec3 0 0 0

-- map f to each coordinate
vmap :: (Scalar -> Scalar) -> Vec3 -> Vec3
vmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

vzip :: Vec3 -> Vec3 -> [(Scalar, Scalar)]
vzip (Vec3 x y z) (Vec3 x' y' z') = [(x,x'), (y,y'), (z,z')]

liftV2 :: (Scalar -> Scalar -> Scalar) -> Vec3 -> Vec3 -> Vec3
liftV2 f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (f x x') (f y y') (f z z')

allCoordsCompare :: (Scalar -> Scalar -> Bool) -> Vec3 -> Vec3 -> Bool
allCoordsCompare f (Vec3 x y z) (Vec3 x' y' z') = x `f` x' && y `f` y' && z `f` z'

allCoordsLE, allCoordsLT :: Vec3 -> Vec3 -> Bool
allCoordsLE = allCoordsCompare (<=)
allCoordsLT = allCoordsCompare (<)

-- scalar product
infixl 7 |*
(|*) :: Scalar -> Vec3 -> Vec3
(|*) a = vmap (a*)

infixl 7 *|
(*|) :: Vec3 -> Scalar -> Vec3
(*|) = flip (|*)

-- dot product
infixl 7 |.|
(|.|) :: Vec3 -> Vec3 -> Scalar
(Vec3 x y z) |.| (Vec3 x' y' z') = x*x' + y*y' + z*z'

-- cross product
infixl 7 |*|
(|*|) :: Vec3 -> Vec3 -> Vec3
(Vec3 x y z) |*| (Vec3 x' y' z') = Vec3 (y*z'-y'*z) (x'*z-x*z') (x*y'-x'*y)

-- component-wise exp
infixr 8 |**|
(|**|) :: Vec3 -> Scalar -> Vec3
(Vec3 x y z) |**| e = Vec3 (x**e) (y**e) (z**e)

-- vector norm
norm :: Vec3 -> Scalar
norm v = sqrt $ v |.| v

-- distance between two vectors
distance :: Vec3 -> Vec3 -> Scalar
distance v w = norm $ v-w

-- normalize vector
normalize :: Vec3 -> Vec3
normalize v
  | n < epsilon     = zerov
  | otherwise       = vmap (/n) v
  where n = norm v

-- Vec3 with identical entries
idv :: Scalar -> Vec3
idv x = Vec3 x x x

-- |Standard basis vectors
basis :: [Vec3]
basis = [Vec3 1 0 0, Vec3 0 1 0, Vec3 0 0 1]


-- 3D ray
data Ray = Ray { origin :: Vec3, direction :: Vec3 } deriving (Eq, Show)

type Velocity = Vec3
type Time = Scalar
-- lorentz boost a vector with velocity. Note c = 1, so |velocity| < 1.
lorentzBoost :: Velocity -> Time -> Vec3 -> Vec3
lorentzBoost velocity t r
  | v >= 1    = error "Superluminal motion forbidden"
  | v < 0.01  = r -- don't bother for v << c
  | otherwise = r + (gamma - 1) * (r |.| n) |* n - gamma * t |* velocity
  where v = norm velocity
        n = vmap (/v) velocity
        gamma = 1 / sqrt (1 - v^2)

-- compute the smallest real root(s) to ax^2 + bx + c = 0
roots2 :: Scalar -> Scalar -> Scalar -> [Scalar]
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
type Axis   = Vec3
type Angle  = Double


-- Homogeneous 4d vector and 4x4 matrix
data Vec4 = Vec4 Scalar Scalar Scalar Scalar deriving (Eq)

instance Show Vec4 where
  show (Vec4 x y z w) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show w ++ ")"

instance Num Vec4 where
  (Vec4 x y z w) + (Vec4 x' y' z' w') = Vec4 (x+x') (y+y') (z+z') (w+w')
  (Vec4 x y z w) * (Vec4 x' y' z' w') = Vec4 (x*x') (y*y') (z*z') (w*w')
  negate (Vec4 x y z w) = Vec4 (-x) (-y) (-z) (-w)
  abs (Vec4 x y z w)    = Vec4 (abs x) (abs y) (abs z) (abs w)
  signum (Vec4 x y z w) = Vec4 (signum x) (signum y) (signum z) (signum w)
  fromInteger n         = let x = fromInteger n in Vec4 x x x x

v4dot :: Vec4 -> Vec4 -> Scalar
v4dot (Vec4 x y z w) (Vec4 x' y' z' w') = x*x' + y*y' + z*z' + w*w'

-- combine a Vec3 with a scalar
infixr 5 |:
(|:) :: Vec3 -> Scalar -> Vec4
(Vec3 x y z) |: w = Vec4 x y z w


-- transform homogenous coordinates (x,y,z,w) -> (x',y',z')
fromHVec4 :: Vec4 -> Vec3
fromHVec4 (Vec4 x y z w)
  | w < epsilon || abs (w-1) < epsilon = Vec3 x y z
  | otherwise                          = Vec3 (x/w) (y/w) (z/w)

data Mat4  = Mat4 Vec4 Vec4 Vec4 Vec4 deriving (Eq)

instance Show Mat4 where
  show (Mat4 a b c d) = "(" ++ show a ++ "\n " ++ show b ++ "\n " ++ show c ++ "\n " ++ show d ++ ")"

instance Num Mat4 where
  (Mat4 x y z w) + (Mat4 x' y' z' w') = Mat4 (x+x') (y+y') (z+z') (w+w')
  
  (Mat4 a1 a2 a3 a4) * (Mat4
                        (Vec4 b11 b12 b13 b14)
                        (Vec4 b21 b22 b23 b24)
                        (Vec4 b31 b32 b33 b34)
                        (Vec4 b41 b42 b43 b44)) =
    Mat4 (Vec4 (a1 `v4dot` b1) (a1 `v4dot` b2) (a1 `v4dot` b3) (a1 `v4dot` b4))
         (Vec4 (a2 `v4dot` b1) (a2 `v4dot` b2) (a2 `v4dot` b3) (a2 `v4dot` b4))
         (Vec4 (a3 `v4dot` b1) (a3 `v4dot` b2) (a3 `v4dot` b3) (a3 `v4dot` b4))
         (Vec4 (a4 `v4dot` b1) (a4 `v4dot` b2) (a4 `v4dot` b3) (a4 `v4dot` b4))
    where b1 = Vec4 b11 b21 b31 b41
          b2 = Vec4 b12 b22 b32 b42
          b3 = Vec4 b13 b23 b33 b43
          b4 = Vec4 b14 b24 b34 b44
          
  negate (Mat4 x y z w) = Mat4 (-x) (-y) (-z) (-w)
  abs (Mat4 x y z w)    = Mat4 (abs x) (abs y) (abs z) (abs w)
  signum (Mat4 x y z w) = Mat4 (signum x) (signum y) (signum z) (signum w)
  fromInteger           = diagMat . fromInteger

diagMat :: Vec4 -> Mat4
diagMat (Vec4 a b c d) =
  Mat4 (Vec4 a 0 0 0)
       (Vec4 0 b 0 0)
       (Vec4 0 0 c 0)
       (Vec4 0 0 0 d)

idMat :: Mat4
idMat = diagMat 1

invMat :: Mat4 -> Maybe Mat4
invMat (Mat4 (Vec4 m0 m1 m2 m3) (Vec4 m4 m5 m6 m7) (Vec4 m8 m9 m10 m11) (Vec4 m12 m13 m14 m15))
  | abs det < epsilon = Nothing
  | otherwise         =
    Just $ Mat4 (Vec4 (i0/det) (i1/det) (i2/det) (i3/det))
                (Vec4 (i4/det) (i5/det) (i6/det) (i7/det))
                (Vec4 (i8/det) (i9/det) (i10/det) (i11/det))
                (Vec4 (i12/det) (i13/det) (i14/det) (i15/det))
  where i0 = m5  * m10 * m15 - 
             m5  * m11 * m14 - 
             m9  * m6  * m15 + 
             m9  * m7  * m14 +
             m13 * m6  * m11 - 
             m13 * m7  * m10

        i4 = -m4  * m10 * m15 + 
               m4  * m11 * m14 + 
               m8  * m6  * m15 - 
               m8  * m7  * m14 - 
               m12 * m6  * m11 + 
               m12 * m7  * m10

        i8 = m4  * m9 * m15 - 
               m4  * m11 * m13 - 
               m8  * m5 * m15 + 
               m8  * m7 * m13 + 
               m12 * m5 * m11 - 
               m12 * m7 * m9

        i12 = -m4  * m9 * m14 + 
                m4  * m10 * m13 +
                m8  * m5 * m14 - 
                m8  * m6 * m13 - 
                m12 * m5 * m10 + 
                m12 * m6 * m9

        i1 = -m1  * m10 * m15 + 
               m1  * m11 * m14 + 
               m9  * m2 * m15 - 
               m9  * m3 * m14 - 
               m13 * m2 * m11 + 
               m13 * m3 * m10

        i5 = m0  * m10 * m15 - 
               m0  * m11 * m14 - 
               m8  * m2 * m15 + 
               m8  * m3 * m14 + 
               m12 * m2 * m11 - 
               m12 * m3 * m10

        i9 = -m0  * m9 * m15 + 
               m0  * m11 * m13 + 
               m8  * m1 * m15 - 
               m8  * m3 * m13 - 
               m12 * m1 * m11 + 
               m12 * m3 * m9

        i13 = m0  * m9 * m14 - 
                m0  * m10 * m13 - 
                m8  * m1 * m14 + 
                m8  * m2 * m13 + 
                m12 * m1 * m10 - 
                m12 * m2 * m9

        i2 = m1  * m6 * m15 - 
               m1  * m7 * m14 - 
               m5  * m2 * m15 + 
               m5  * m3 * m14 + 
               m13 * m2 * m7 - 
               m13 * m3 * m6

        i6 = -m0  * m6 * m15 + 
               m0  * m7 * m14 + 
               m4  * m2 * m15 - 
               m4  * m3 * m14 - 
               m12 * m2 * m7 + 
               m12 * m3 * m6

        i10 = m0  * m5 * m15 - 
                m0  * m7 * m13 - 
                m4  * m1 * m15 + 
                m4  * m3 * m13 + 
                m12 * m1 * m7 - 
                m12 * m3 * m5

        i14 = -m0  * m5 * m14 + 
                m0  * m6 * m13 + 
                m4  * m1 * m14 - 
                m4  * m2 * m13 - 
                m12 * m1 * m6 + 
                m12 * m2 * m5

        i3 = -m1 * m6 * m11 + 
               m1 * m7 * m10 + 
               m5 * m2 * m11 - 
               m5 * m3 * m10 - 
               m9 * m2 * m7 + 
               m9 * m3 * m6;

        i7 = m0 * m6 * m11 - 
               m0 * m7 * m10 - 
               m4 * m2 * m11 + 
               m4 * m3 * m10 + 
               m8 * m2 * m7 - 
               m8 * m3 * m6;

        i11 = -m0 * m5 * m11 + 
                m0 * m7 * m9 + 
                m4 * m1 * m11 - 
                m4 * m3 * m9 - 
                m8 * m1 * m7 + 
                m8 * m3 * m5;

        i15 = m0 * m5 * m10 - 
                m0 * m6 * m9 - 
                m4 * m1 * m10 + 
                m4 * m2 * m9 + 
                m8 * m1 * m6 - 
                m8 * m2 * m5

        det = m0 * i0 + m1 * i4 + m2 * i8 + m3 * i12


transformVec4 :: Mat4 -> Vec4 -> Vec4
transformVec4 (Mat4 a1 a2 a3 a4) v = Vec4 (a1 `v4dot` v) (a2 `v4dot` v) (a3 `v4dot` v) (a4 `v4dot` v)
        
-- standard transformation matricies
scaleMat :: Vec3 -> Mat4
scaleMat v = diagMat $ v |: 1

translateMat :: Vec3 -> Mat4
translateMat (Vec3 x y z) =
  Mat4 (Vec4 1 0 0 x)
       (Vec4 0 1 0 y)
       (Vec4 0 0 1 z)
       (Vec4 0 0 0 1)

-- rotate CCW along axis
rotateMat :: Angle -> Axis -> Mat4
rotateMat angle axis =
  Mat4 (Vec4 (x*x + (1-x*x)*c) (x*y*(1-c) - z*s) (x*z*(1-c) + y*s) 0)
       (Vec4 (x*y*(1-c) + z*s) (y*y + (1-y*y)*c) (y*z*(1-c) - x*s) 0)
       (Vec4 (x*z*(1-c) - y*s) (y*z*(1-c) + x*s) (z*z + (1-z*z)*c) 0)
       (Vec4 0                 0                 0                 1)
  where (Vec3 x y z) = normalize axis
        s = sin angle
        c = cos angle
