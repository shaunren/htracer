import Tracer
import Scene
import Surfaces.Sphere as S
import Surfaces.Plane as P
import Lights
import Math


checkedMatUV :: Double -> UVMap
checkedMatUV scale u v
  | (odd . truncate $ u*scale) == (odd . truncate $ v*scale) = makeMaterial emission white 0.2 0.8 0.6 6 zerov False 1 zerov
  | otherwise                                                = makeMaterial emission black 0.2 0.8 0 3 zerov False 1 zerov
  where emission = 0.1|*cyan

view :: View
view = View { cameraPos = Vec3 0 0 5
            , lookingAt = Vec3 0 0 0
            , upVector  = Vec3 0 1 0
            , hFov      = 70 }

scene :: Scene
scene = Scene { bgColour      = black
              , ambientColour = idv 0.05
              , airRefractInd = 1
              , lights        = [ --parallelLight True (Vec3 1 (-1) (-1)) (idv 0.5)
                                 pointLight True 3 2 (Vec3 1 4 5) lightcyan
                                , pointLight True 4 2 (Vec3 3 5 1) (idv 0.9)]
              , surfaces      = [ (SW (S.Sphere (Vec3 (-1.1) 1.1 (-2)) 2.2
                                       (constUVMap $
                                        Material {
                                          emission      = black
                                        , ambientK      = red
                                        , diffuseK      = red
                                        , specularK     = idv 0.5
                                        , shininess     = 20
                                        , transmissionK = black
                                        , diaelectric   = False
                                        , refractiveInd = 1
                                        , absorption    = black })))
                                , (SW (S.Sphere (Vec3 (2.7) 1 (-2.5)) 1.3 (checkedMatUV 25)))
                                , (SW (S.Sphere (Vec3 2 0.6 (-0.5)) 0.6
                                      (constUVMap $
                                       (Material {
                                           emission      = black
                                         , ambientK      = (0.15|*cyan)
                                         , diffuseK      = (0.15|*cyan)
                                         , specularK     = black
                                         , shininess     = 0
                                         , transmissionK = white
                                         , diaelectric   = True
                                         , refractiveInd = 1.1
                                         , absorption    = Vec3 0.3 0.8 0.99 }))))
                                , (SW (P.Plane (Vec3 0 (-1.5) 0) (Vec3 0 1 0)
                                      (Material black (idv 0.8) (idv 0.8) white 15 0 False 1 black)))
                                --, (SW (P.Plane (Vec3 0 0 (-20)) (Vec3 0 0 1)
                                --      (Material black (0.7|*green) (0.7|*green) (idv 0.1) 5 0 False 1 zerov)))
                                ]
              }

makePpm :: Int -> Int -> [Colour] -> String
makePpm w h pixels = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" ++ (unwords . map toRGB $ pixels)
  where toRGB (Vec3 r g b) = (rgb' r) ++ " " ++ (rgb' g) ++ " " ++ (rgb' b)
        rgb' x  = show $ round (x*255)

width,height,maxdepth :: Int
width    = 1024
height   = 768
maxdepth = 16

multisampler :: Multisampler
multisampler = msaa4

main :: IO ()
main = do
  putStrLn . makePpm width height $ render view perspectiveProjection multisampler scene width height maxdepth
