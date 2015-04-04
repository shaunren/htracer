module Tracer where

import Control.Applicative
import Control.Parallel.Strategies

import Math
import Scene
import Multisampler (Multisampler)

-- render a scene
render :: View -> Projection -> Multisampler -> Scene -> Width -> Height -> Int -> [Colour]
render view projection ms scene@(Scene _ _ airn _ _) width height maxdepth =
  parMap rdeepseq (combine . map projAndTrace . (ms vright vup)) viewPlane
  where
    (viewPlane,vright,vup) = makeViewPlane view width height
    projAndTrace           = trace maxdepth scene airn . projection view
    combine cs             = clamp $ 1/l |* sum cs
      where l = fromIntegral $ length cs

-- trace a single ray
trace :: Int -> Scene -> Double -> Ray -> Colour
trace depth _ _ _ | depth < 0  = black     -- maximum recursion depth reached
trace depth scene@(Scene bg ambient airn ls ss) rn ray@(Ray o d) =
  case closestIntersection ss ray of -- get closest intersection
    Nothing        -> bg
    Just (t, SW s) -> ke <+> ka*ambient <+> lightc <+> reflectc <+> transmitc
      where pt = (o + d*|t)
            n  = getNormal s pt
            mat@(Material ke ka _ ks _ kt diaelec matn absorbe) = getMaterial s pt

            -- calculate reflection and transmission (refraction)
            nd    = n |.| d
            n2    = if nd<=0 then matn else airn
            cosin = abs nd
            (fr, sintrans2) = if diaelec then fresnelReflectance rn n2 cosin else (1,0)

            beerk    = if nd>0 then absorbe|**|t else 1       -- Beer's law
            
            reflectk = fr |* beerk * ks
            reflectc = if   norm reflectk > 0.002
                       then reflectk * trace (depth-1) scene rn (Ray pt $ reflection d n)
                       else black
                            
            nrat      = rn/n2
            transv    = nrat|*d - (signum nd) * (nrat*cosin - sqrt (1-sintrans2)) |* n
            transk    = (1-fr) |* beerk * ((idv 1) - ks) * kt
            transmitc = if norm transk > 0.002
                        then transk * trace (depth-1) scene n2 (Ray pt transv)
                        else black

             -- diffuse & specular; don't calculate if in surface
            lightc   = if nd>0 then black else sumColour $ ls <*> [ss] <*> [mat] <*> [negate d] <*> [pt] <*> [n]
            
