{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List.Split
import GHC.Real (fromIntegral)

purple' :: (Ord a, Floating a) => Colour a
--purple' = sRGB24read "#6833ff"
purple' = sRGB 0.4 0.2 1

blue' :: (Ord a, Floating a) => Colour a
blue' = sRGB 0.22 0.57 0.95

myCircle :: Diagram B
myCircle = circle 1 # fc white

myCircles :: Double -> Diagram B
myCircles x = mkCircles x red blue'
  === mkCircles x orange teal
  === mkCircles x yellow green

mkCircles :: Double -> Colour Double -> Colour Double -> Diagram B
mkCircles x c1 c2 = foldr1 (|||) [circle 2
                            # fc (blend  (1-n*(1/x)) c1 c2)
                            # frame (x/20)
                            | n <- [0..x]]
squareSpiral :: Diagram B
squareSpiral = foldr1 atop $ [ rect n (n+0)
                               # fc (blend ((8-n)/6) purple' black)
                               # lc (blend ((8-n)/8) black blue' )
                               # rotate (n*60 @@ deg) | n <- 0.1 : [0.5,1..9]]


-- metapolygons  
mkMetaPolygon :: Int  -> Diagram B
mkMetaPolygon n = atPoints polyPath
--                  $ mapGradient red blue lc
                  $ replicate n $ frame (fromIntegral n/20) p
  where
    polyPath = trailVertices (regPoly (fromIntegral n) 1)
    p = regPoly (fromIntegral n) 1

mkMetaPolygons :: Int  -> [Diagram B]
mkMetaPolygons n = [mkMetaPolygon x | x <- [3..n]]
-- / metapolygons  

mapGradient ::  Colour Double -> Colour Double
  -> (Colour Double -> Diagram B -> Diagram B)
  -> [Diagram B] -> [Diagram B]

mapGradient c1 c2 f ds = zipWith f cs ds
  where cs = [ blend r c1 c2 | r <- [0, inc ..]]
        inc = 1/fromIntegral (length ds-1)

listToGrid :: [a] -> [[a]]
listToGrid l = chunksOf (round n) l
  where n = sqrt $ fromIntegral $  (^2)
          $ ceiling $ sqrt $ fromIntegral $ length l

normalizeDiagrams :: [Diagram B] -> [Diagram B]
normalizeDiagrams ds = map (sized d) ds
  where d = dims2D x y
        x = maximum . map width $ ds
        y = maximum . map height $ ds


diagramGrid :: [Diagram B] -> Diagram B
diagramGrid =  vcat .  map hcat . listToGrid

main :: IO ()
main = mainWith $ frame 0.2 $ diagramGrid
       $ normalizeDiagrams
       $ mkMetaPolygons 11
