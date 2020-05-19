module Compressor
    ( compressor,
      showClusters,
      initClusters,
      euclideanDistance,
      hasConverged,
      fillCluster,
      clusterMean
    ) where

import ArgumentManager (ConvergenceLimit (ConvergenceLimit), ColorLimit (ColorLimit))
import ImageDefinition.Color (Color (Color), R(..), G(..), B(..), FloatFractionable (..))
import ImageDefinition.Pixel (Pixel(Pixel))
import ImageDefinition.Cluster
import Exception (ICExceptions (RuntimeException))

import Data.List (sortOn)
import Control.Exception (throw)

type OldCluster = Cluster
type NewCluster = Cluster
type Distance   = Float

initClusters :: [Pixel] -> ColorLimit -> [Cluster]
initClusters = initClusters' []

{-# INLINE initClusters' #-}
initClusters' :: [Cluster] -> [Pixel] -> ColorLimit -> [Cluster]
initClusters' l  _                      (ColorLimit 0) = l
initClusters' _  []                     _              = throw $ RuntimeException "Not enough different colors to make clusters."
initClusters' l  (pix@(Pixel _ col):xs) lim            | isColorInCluster col l = initClusters' l xs lim
                                                       | otherwise              = initClusters' (Cluster col col [pix]:l) xs $ lim - 1

isColorInCluster :: Color -> [Cluster] -> Bool
isColorInCluster _   []                    = False
isColorInCluster col (Cluster ccol _ _:xs) | col == ccol = True
                                           | otherwise   = isColorInCluster col xs

compressor :: [Cluster] -> [Pixel] -> ConvergenceLimit -> [Cluster]
compressor cl pix = compressor' (createNewClusters pix cl) [] pix

{-# INLINE compressor' #-}
compressor' :: [OldCluster] -> [NewCluster] -> [Pixel] -> ConvergenceLimit -> [Cluster]
compressor' old []  pix lim = compressor' old (createNewClusters pix old) pix lim
compressor' old new pix lim | hasConverged lim new = old
                            | otherwise            = compressor' new [] pix lim

createNewClusters :: [Pixel] -> [OldCluster] -> [NewCluster]
createNewClusters pix = fillCluster pix . clusterMean

fillCluster :: [Pixel] -> [Cluster] -> [NewCluster]
fillCluster pixels clusters = foldl assignCluster clusters pixels

assignCluster :: [Cluster] -> Pixel -> [Cluster]
assignCluster cluster pix = assignCluster' pix $ sortOn (clusterEuclideanDistance pix) cluster

{-# INLINE assignCluster' #-}
assignCluster' :: Pixel -> [Cluster] -> [Cluster]
assignCluster' _   []                    = throw $ RuntimeException "Impossible happened."
assignCluster' pix (Cluster oav av l:xs) = Cluster oav av (pix:l) : xs

clusterEuclideanDistance :: Pixel -> Cluster -> Distance
clusterEuclideanDistance (Pixel _ pixcol) (Cluster cluscol _ _) = euclideanDistance pixcol cluscol

euclideanDistance :: Color -> Color -> Distance
euclideanDistance (Color (R x1) (G y1) (B z1)) (Color (R x2) (G y2) (B z2)) = sqrt $ ((x1 - x2)^2) + ((y1 - y2)^2) + ((z1 - z2)^2)

clusterMean :: [Cluster] -> [Cluster]
clusterMean = map clusterMean'

{-# INLINE clusterMean' #-}
clusterMean' :: Cluster -> Cluster
clusterMean' (Cluster old _ pix) = Cluster (colorMean pix) old []

colorMean :: [Pixel] -> Color
colorMean p = sumColorOfPixels p `divColor` fromIntegral (length p)

sumColorOfPixels :: [Pixel] -> Color
sumColorOfPixels = foldl sumExtractedColor (Color 0 0 0)

sumExtractedColor :: Color -> Pixel -> Color
sumExtractedColor c (Pixel _ color) = c + color

divColor :: Color -> Float -> Color
divColor (Color r g b) len = Color (len `divide` r) (len `divide` g) (len `divide` b)

hasConverged :: ConvergenceLimit -> [NewCluster] -> Bool
hasConverged _                        []                       = True
hasConverged l@(ConvergenceLimit lim) (Cluster ocol ncol _:xs) | euclideanDistance ocol ncol > lim = False
                                                               | otherwise                         = hasConverged l xs
