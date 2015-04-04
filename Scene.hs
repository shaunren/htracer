{-# LANGUAGE ExistentialQuantification #-}
module Scene where

import Data.Word (Word8)
import Math
import Data.List (foldl',foldl1')

-- We use OpenGL coordinate system (X->right, Y^up, Zo out)

-- stuff related to surfaces (colour, material, etc)
type Colour = Vec3

clamp :: Colour -> Colour
clamp = vmap (min 1)

-- add colour
(<+>) :: Colour -> Colour -> Colour
a <+> b = clamp (a+b)

-- sum colour
sumColour :: [Colour] -> Colour
sumColour = foldl1' (<+>)

-- some basic colors
black,white,red,green,blue,yellow,magenta,cyan :: Colour
black   = Vec3 0 0 0
white   = Vec3 1 1 1
red     = Vec3 1 0 0
green   = Vec3 0 1 0
blue    = Vec3 0 0 1
yellow  = Vec3 1 1 0
magenta = Vec3 1 0 1
cyan    = Vec3 0 1 1

lightcyan = Vec3 0.8 1 1

data Material = Material { emission      :: Colour
                         , ambientK      :: Colour
                         , diffuseK      :: Colour
                         , specularK     :: Colour
                         , shininess     :: Word8
                         , transmissionK :: Colour
                         , diaelectric   :: Bool
                         , refractiveInd :: Double }
              deriving (Eq, Show)

makeMaterial :: Colour -> Colour -> Double -> Double -> Double -> Word8 -> Colour -> Bool -> Double -> Material
makeMaterial e c ka kd ks = Material e (ka|*c) (kd|*c) (ks|*c)

class Surface s where
  -- get closest intersection point (o+dt) such that d>0, if there is any
  intersect :: s -> Ray -> Maybe Double
  -- compute unit normal at point
  getNormal :: s -> Point -> Normal
  -- get material at point
  getMaterial :: s -> Point -> Material
                        
-- ADT wrapping surface for heterogeneous surface list
data SurfaceW = forall s. Surface s => SW s


-- does ray intersect any surface
intersectAny :: [SurfaceW] -> Ray -> Bool
intersectAny [] _ = False
intersectAny ((SW s):ss) ray = case (intersect s ray) of
  Nothing -> intersectAny ss ray
  Just t  -> if t>0.0001 then True else intersectAny ss ray -- the ray might intersect itself

-- closest intersection of a ray to a surface
closestIntersection :: [SurfaceW] -> Ray -> Maybe (Double, SurfaceW)
closestIntersection ss ray = foldl' f Nothing ss
  where f a sw@(SW s)  = maybe a (g sw a) $ intersect s ray
        g sw Nothing t = Just (t,sw)
        g sw a@(Just (t', _)) t
          | t' > t    = Just (t,sw)
          | otherwise = a
                        

-- a UV map, where (u,v) in [0,1]^2
type UVMap = Double -> Double -> Material

-- a const uvmap
constUVMap :: Material -> UVMap
constUVMap mat _ _ = mat

-- light :: surfaces -> material -> (v from pt to camera) -> point -> normal -> (colour of point)
type Light = [SurfaceW] -> Material -> Vec3 -> Point -> Normal -> Colour

-- Fresnel reflectance, Schlick's approximation
-- 0 <= cosInAngle <= 1
-- evaluates to (R(\theta_i), sin^2(\theta_t))
fresnelReflectance :: Double -> Double -> Double -> (Double,Double)
fresnelReflectance n1 n2 cosInAngle 
  | n1 <= n2                      = (r + (1-r)*((1-cosInAngle)^5), sinTransAngleSq)
  | cosInAngleSq > cosCritAngleSq = (r + (1-r)*((1-sqrt cosTransAngleSq)^5), sinTransAngleSq)
  | otherwise                     = (1, sinTransAngleSq) -- total internal reflection
  where r                = ((n1-n2)/(n1+n2))^2
        cosInAngleSq     = cosInAngle^2
        cosCritAngleSq   = 1-(n2/n1)^2
        sinTransAngleSq  = (n1/n2)^2 * (1-cosInAngleSq)
        cosTransAngleSq  = 1 - sinTransAngleSq

--
data Scene = Scene { bgColour      :: Colour
                   , ambientColour :: Colour
                   , airRefractInd :: Double
                   , lights        :: [Light]
                   , surfaces      :: [SurfaceW] }

data View = View { cameraPos  :: Vec3
                 , viewDist   :: Double
                 , lookingAt  :: Vec3
                 , upVector   :: Vec3
                 } deriving (Eq, Show)

type Width = Int
type Height = Int

makeViewPlane :: View -> Width -> Height -> [Point]
makeViewPlane (View cpos dist viewdir up) w h =
  [  center
   + ((0.5+(fromIntegral x)-width/2)  |* vright)
   + ((0.5+(fromIntegral y)-height/2) |* vup)    | y <- [h-1,h-2..0], x <- [0..w-1] ]
  
  where width  = fromIntegral w
        height = fromIntegral h
        vdir   = normalize (viewdir-cpos)
        vup    = normalize up
        vright = vdir |*| vup
        center = cpos + dist |* vdir
        

type Projection = View -> Point -> Ray
-- predefined projections
orthographicProjection :: Projection
orthographicProjection (View cpos _ vdir _) p = Ray p $ normalize (vdir-cpos)

perspectiveProjection :: Projection
perspectiveProjection (View cpos _ _ _) p = Ray cpos $ normalize (p-cpos)