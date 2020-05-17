module ImageDefinition.Cluster
    ( Cluster (..),
      showClusters,
    ) where

import ImageDefinition.Pixel (showPixels, Pixel)
import ImageDefinition.Color (Color)

type NewAverageColor = Color
type OldAverageColor = Color

data Cluster = Cluster NewAverageColor OldAverageColor [Pixel]

showClusters :: [Cluster] -> [String]
showClusters []                     = []
showClusters (Cluster col _ pix:xs) = showClusters xs ++ ["--\n" ++ show col ++ "\n-"] ++ showPixels pix
