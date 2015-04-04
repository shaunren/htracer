module Lights (parallelLight, pointLight) where

import Data.Word(Word8)
import Math
import Scene

---- Phong illumination (diffuse + specular)
-- v: vector front point to viewer
-- l: vector from point to light
-- n: normal at point
phongIllum :: Vec3 -> Vec3 -> Vec3 -> Colour -> Colour -> Material -> Colour
phongIllum v l n cdiffuse cspecular (Material _ _ kd ks sh _ _ _ _) =
      ((kd * cdiffuse)  *| max (n |.| l) 0)
  <+> ((ks * cspecular) *| (max (r |.| v) 0)^sh)
  where r = reflection (negate l) n

-- light attenuation factor
attenuation :: Double -> Word8 -> Double -> Double
attenuation fadeDist fadePower d = if fadePower < 1 then 1 else 2/(1 + (d/fadeDist)^fadePower)



-- parallel light source (e.g. sunlight)
parallelLight :: Bool -> Vec3 -> Colour -> Light
parallelLight shadow ldir lcolour ss mat v pt n
  | shadow && intersectAny ss (Ray pt ld) = black
  | otherwise = phongIllum v ld n lcolour lcolour mat
  where ld = negate $ normalize ldir

-- point light source (e.g. light bulb)
pointLight :: Bool -> Double -> Word8 -> Point -> Colour -> Light
pointLight shadow fadeDist fadePower lpos lcolour ss mat v pt n
  | d<epsilon || (shadow && intersectAny ss (Ray pt ld)) = black
  | otherwise = phongIllum v ld n lc lc mat
  where ld' = lpos - pt
        d   = norm ld'
        ld  = if d<epsilon then zerov else (1/d)|*ld'
        lc  = attenuation fadeDist fadePower d |* lcolour

