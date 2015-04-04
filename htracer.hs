import Tracer
import Scene
import Surfaces.Sphere as S
import Surfaces.Plane as P
import Lights
import Math
import Data.List (unwords)


checkedMatUV :: Double -> UVMap
checkedMatUV scale u v
  | (odd . truncate $ u*scale) == (odd . truncate $ v*scale) = makeMaterial emission white 0.2 0.8 0.6 6 zerov False 1
  | otherwise                                                = makeMaterial emission black 0.2 0.8 0 3 zerov False 1
  where emission = 0.1|*cyan

view :: View
view = View { cameraPos = Vec3 0 0 (800)
            , viewDist  = 800
            , lookingAt = Vec3 0 0 (-1)
            , upVector  = Vec3 0 1 0 }

scene :: Scene
scene = Scene { bgColour      = black
              , ambientColour = idv 0.05
              , airRefractInd = 1
              , lights        = [ --parallelLight True (Vec3 1 (-1) (-1)) (idv 0.5)
                                 pointLight True 450 2 (Vec3 100 400 500) lightcyan
                                , pointLight True 500 2 (Vec3 300 500 100) (idv 0.9)]
              , surfaces      = [ (SW (S.Sphere (Vec3 (-100) 110 (-150)) 220
                                       (constUVMap (Material black red red (idv 0.5) 20 (idv 0.5) False 1))))
                                , (SW (S.Sphere (Vec3 (270) 100 (-120)) 130 (checkedMatUV 25)))
                                , (SW (S.Sphere (Vec3 70 10 500) 30
                                      (constUVMap (Material black black (0.05|*yellow) zerov 10 (idv 1) True 1.03))))
                                , (SW (P.Plane (Vec3 0 (-150) 0) (Vec3 0 1 0)
                                      (Material black (idv 0.8) (idv 0.8) (idv 1) 15 0 False 1)))
                                --, (SW (P.Plane (Vec3 0 0 (-2000)) (Vec3 0 0 1)
                                --      (Material black (0.7|*green) (0.7|*green) (idv 0.1) 5 0 False 1)))
                                ]
              }

makePpm :: Int -> Int -> [Colour] -> String
makePpm w h pixels = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" ++ (unwords . map toRGB $ pixels)
  where toRGB (Vec3 r g b) = (rgb' r) ++ " " ++ (rgb' g) ++ " " ++ (rgb' b)
        rgb' x  = show $ round (x*255)

width,height,maxdepth :: Int
width    = 800
height   = 600
maxdepth = 16

multisampler :: Multisampler
multisampler = msaa4

main :: IO ()
main = do
  putStrLn . makePpm width height $ render view perspectiveProjection multisampler scene width height maxdepth
