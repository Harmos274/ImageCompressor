module Compressor
    ( compressor,
      showCluster,
      initClusters,
      Cluster (..)
    ) where

import ArgumentManager (ConvergenceLimit (..), ColorLimit (..))
import ImageDefinition (Color (Color), Pixel (Pixel), Position(..), showColor, showPixels)
import Exception (ICExceptions(RuntimeException))

import Data.List (sortOn)
import Control.Exception (throw)

type AverageColor = Color Float
type OldCluster = Cluster
type NewCluster = Cluster

data Cluster = Cluster AverageColor [Pixel]

showCluster :: [Cluster] -> [String]
showCluster [] = []
showCluster (Cluster col pix:xs) = ["--", showColor col, "-"] ++ showPixels pix ++ showCluster xs

isColorInCluster :: Color Float -> [Cluster] -> Bool
isColorInCluster col [] = False
isColorInCluster col (Cluster ccol _:xs) | col == ccol = True
                                         | otherwise   = isColorInCluster col xs

initClusters :: [Pixel] -> ColorLimit -> [Cluster]
initClusters pix clim@(ColorLimit lim) | length pix >= lim = initClusters' [] pix clim
                                       | otherwise = throw $ RuntimeException "Not enough different colors to make clusters."

initClusters' :: [Cluster] -> [Pixel] -> ColorLimit -> [Cluster]
initClusters' l _  (ColorLimit 0) = l
initClusters' _ [] _              = throw $ RuntimeException "Not enough pixels to make clusters."
initClusters' [] (pix@(Pixel _ col):xs) (ColorLimit lim) = initClusters' [Cluster col [pix]] xs (ColorLimit (lim - 1))
initClusters' l  (pix@(Pixel _ col):xs) clim@(ColorLimit lim) | isColorInCluster col l = initClusters' l xs clim
                                                              | otherwise              = initClusters' (l ++ [Cluster col [pix]]) xs (ColorLimit (lim - 1))

compressor :: [Cluster] -> [Pixel] -> ConvergenceLimit -> [Cluster]
compressor old pix limit | hasConverged limit old new = old
                         | otherwise = compressor new pix limit
                         where new = fillCluster pix $ clusterMean old

fillCluster :: [Pixel] -> [Cluster] -> [Cluster]
fillCluster pixels clusters = foldl assignCluster clusters pixels

assignCluster :: [Cluster] -> Pixel -> [Cluster]
assignCluster cluster pix =  assignCluster' pix $ sortOn (clusterEuclideanDistance pix) cluster

euclideanDistance :: Color Float -> Color Float -> Float
euclideanDistance (Color r1 g1 b1) (Color r2 g2 b2) = sqrt $ ((r1 - r2)^2) + ((g1 - g2)^2) + ((b1 - b2)^2)

clusterEuclideanDistance :: Pixel -> Cluster -> Float
clusterEuclideanDistance (Pixel _ pixcol) (Cluster cluscol _) = euclideanDistance pixcol cluscol

assignCluster' :: Pixel -> [Cluster] -> [Cluster]
assignCluster' pix ((Cluster av l):xs) = Cluster av (pix:l) : xs

clusterMean :: [Cluster] -> [Cluster]
clusterMean = map clusterMean'

clusterMean' :: Cluster -> Cluster
clusterMean' (Cluster _ pix) = Cluster (colorMean pix) []

colorMean :: [Pixel] -> Color Float
colorMean [] = Color 0 0 0
colorMean pl@(Pixel _ color:xs) = divColor (sumColor $ extractColor pl) $ length pl

extractColor :: [Pixel] -> [Color Float]
extractColor [] = []
extractColor (Pixel _ color:xs) = color : extractColor xs

sumColor :: [Color Float] -> Color Float
sumColor = foldr (+) (Color 0 0 0)

divColor :: Color Float -> Int -> Color Float
divColor (Color r g b) len = Color (r / fromIntegral len) (g / fromIntegral len) (b / fromIntegral len)

hasConverged :: ConvergenceLimit -> [OldCluster] -> [NewCluster] -> Bool
hasConverged _ [] [] = True
hasConverged clim@(ConvergenceLimit lim) ((Cluster ocol _):oxs) ((Cluster ncol _):nxs) = (euclideanDistance ocol ncol <= lim) && hasConverged clim oxs nxs
