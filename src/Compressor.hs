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

type AverageColor = Color Int
type OldCluster = Cluster
type NewCluster = Cluster

data Cluster = Cluster AverageColor [Pixel]

showCluster :: [Cluster] -> [String]
showCluster [] = []
showCluster (Cluster col pix:xs) = ["--", showColor col, "-"] ++ showPixels pix ++ showCluster xs

initClusters :: [Pixel] -> ColorLimit -> [Cluster]
initClusters [] _ = throw $ RuntimeException "Empty pixel list."
initClusters _ (ColorLimit 0) = []
initClusters (pix@(Pixel pos col):xs) (ColorLimit lim) = Cluster col [pix] : initClusters xs (ColorLimit (lim - 1))

compressor :: [Cluster] -> [Pixel] -> ConvergenceLimit -> [Cluster]
compressor old pix limit | hasConverged limit old new = new
                         | otherwise = compressor new pix limit
                         where new = fillCluster pix $ clusterMean old

fillCluster :: [Pixel] -> [Cluster] -> [Cluster]
fillCluster pixels clusters = foldl assignCluster clusters pixels

assignCluster :: [Cluster] -> Pixel -> [Cluster]
assignCluster cluster pix =  assignCluster' pix $ sortOn (clusterEuclideanDistance pix) cluster

euclideanDistance :: Color Int -> Color Int -> Float
euclideanDistance (Color r1 g1 b1) (Color r2 g2 b2) = sqrt . fromIntegral $ ((r1 - r2)^2) + ((g1 - g2)^2) + ((b1 - b2)^2)

clusterEuclideanDistance :: Pixel -> Cluster -> Float
clusterEuclideanDistance (Pixel _ pixcol) (Cluster cluscol _) = euclideanDistance pixcol cluscol

assignCluster' :: Pixel -> [Cluster] -> [Cluster]
assignCluster' pix ((Cluster av l):xs) = Cluster av (pix:l) : xs

clusterMean :: [Cluster] -> [Cluster]
clusterMean = map clusterMean'

clusterMean' :: Cluster -> Cluster
clusterMean' (Cluster _ pix) = Cluster (colorMean pix) []

colorMean :: [Pixel] -> Color Int
colorMean [] = Color 0 0 0
colorMean pl@(Pixel _ color:xs) = divColor (sumColor $ extractColor pl) $ length pl

extractColor :: [Pixel] -> [Color Int]
extractColor [] = []
extractColor (Pixel _ color:xs) = color : extractColor xs

sumColor :: [Color Int] -> Color Int
sumColor = foldr (+) (Color 0 0 0)

divColor :: Color Int -> Int -> Color Int
divColor (Color r g b) len = Color (r `div` len) (g `div` len) (b `div` len)

hasConverged :: ConvergenceLimit -> [OldCluster] -> [NewCluster] -> Bool
hasConverged _ [] [] = True
hasConverged clim@(ConvergenceLimit lim) ((Cluster ocol _):oxs) ((Cluster ncol _):nxs) = (euclideanDistance ocol ncol <= lim) && hasConverged clim oxs nxs
