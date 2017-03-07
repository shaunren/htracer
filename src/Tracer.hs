{-# LANGUAGE TypeFamilies #-}
module Tracer (render, trace) where

import Control.Applicative
import Control.Parallel.Strategies
import System.Random (RandomGen)
import Control.Monad.Random

import Math
import Scene
import Multisampler (Multisampler)

-- render a scene
render :: (RandomGen g) => View -> Projection g -> Multisampler -> Scene -> Width -> Height -> Int -> g -> [Colour]
render view projection ms scene@(Scene _ _ airn _ _) width height maxdepth rng =
  map (\(pt,g) -> combine (evalRand (traceMsFun pt) g)) (zip viewPlane (splitRandomGens rng (length viewPlane)))
  `using` parListChunk chunkSize rdeepseq  -- chunk work so we don't overflow spark pool
  where
    (viewPlane,vright,vup) = makeViewPlane view width height
    projAndTrace pt        = projection view pt >>= return . trace maxdepth scene airn
    traceMsFun               = mapM projAndTrace . ms vright vup
    combine cs             = clamp $ 1/l |* sum cs
      where l = fromIntegral $ length cs
    imageArea              = width * height
    chunkSize              = max 512 . round . sqrt $ fromIntegral imageArea


data TraceState = TraceState { scene :: Scene
                             , refactiveIndex :: Scalar
                             , depth :: Int
                             }

-- trace a single ray
trace :: Int -> Scene -> Scalar -> Ray -> Colour
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
            rd       = reflection d n
            reflectc = if   norm reflectk > 0.002
                       then reflectk * trace (depth-1) scene rn (Ray (pt+0.0001|*rd) rd)
                       else black
                            
            nrat      = rn/n2
            transv    = nrat|*d - (signum nd) * (nrat*cosin - sqrt (1-sintrans2)) |* n
            transk    = (1-fr) |* beerk * ((idv 1) - ks) * kt
            transmitc = if norm transk > 0.002
                        then transk * trace (depth-1) scene n2 (Ray (pt+0.0001|*transv) transv)
                        else black

             -- diffuse & specular; don't calculate if in surface
            lightc   = if nd>0 then black else sumColour $ ls <*> [ss] <*> [mat] <*> [negate d] <*> [pt] <*> [n]

splitRandomGens :: (RandomGen g) => g -> Int -> [g]
splitRandomGens g n =
  (iterate (\(g':gs) -> let (ga,gb) = split g' in ga:gb:gs) [g]) !! (n-1)
