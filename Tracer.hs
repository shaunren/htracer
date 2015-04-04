module Tracer where

import Control.Applicative
import Control.Parallel.Strategies

import Math
import Scene

-- multisampling transformer
-- vup -> vright -> pt -> [pts]
type Multisampler = Vec3 -> Vec3 -> Point -> [Point]

-- supersample point offsets
msaa :: [(Double,Double)] -> Multisampler
msaa offsets vup vright p = [p + (x|*vright) + (y|*vup) | (x,y) <- offsets]

noaa, msaa4 :: Multisampler
noaa _ _ = pure
msaa4 = msaa [(0.125,0.375),(0.375,-0.125),(-0.125,-0.375),(-0.375,0.125)] -- rotated grid pattern

-- render a scene
render :: View -> Projection -> Multisampler -> Scene -> Width -> Height -> Int -> [Colour]
render view@(View cpos _ viewdir up) projection ms scene@(Scene _ _ airn _ _) width height maxdepth =
  parMap rdeepseq (combineColour . map projAndTrace . (ms vup vright)) $ makeViewPlane view width height
  where
    vdir             = normalize (viewdir-cpos)
    vup              = normalize up
    vright           = vdir |*| vup
    projAndTrace     = trace maxdepth scene airn . projection view
    combineColour cs = clamp . vmap (/l) $ sum cs
      where l = fromIntegral $ length cs

-- trace a single ray
trace :: Int -> Scene -> Double -> Ray -> Colour
trace depth _ _ _ | depth < 0  = black     -- maximum recursion depth reached
trace depth scene@(Scene bg ambient airn ls ss) rn ray@(Ray o d) =
  case closestIntersection ss ray of -- get closest intersection
    Nothing        -> bg
    Just (t, SW s) -> ke <+> (ka*ambient) <+> lightC <+> reflectC <+> transmitC
      where pt = (o + d*|t)
            n  = getNormal s pt
            mat@(Material ke ka _ ks _ kt diaelec matn) = getMaterial s pt

            -- calculate reflection and transmission (refraction)
            nd    = n |.| d
            n2    = if nd<=0 then matn else airn
            cosin = abs nd
            (fr, sintrans2) = if diaelec then fresnelReflectance rn n2 cosin else (1,0)
            
            reflectC = if   fr * norm ks > 0.003
                       then fr |* (ks * trace (depth-1) scene rn (Ray pt $ reflection d n))
                       else black
                            
            nrat      = rn/n2
            transv    = (nrat |* d) - ((signum nd) *  (nrat*cosin - sqrt (1-sintrans2))) |* n
            transk    = (1-fr) |* ((idv 1) - ks) * kt
            transmitC = if norm transk > 0.003 then transk * trace (depth-1) scene n2 (Ray (pt+0.00001|*transv) transv) else black

             -- diffuse & specular; don't calculate if in surface
            lightC   = if nd>0 then black else sumColour $ ls <*> [ss] <*> [mat] <*> [negate d] <*> [pt] <*> [n]
            
