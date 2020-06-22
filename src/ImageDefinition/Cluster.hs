module ImageDefinition.Cluster
    ( Cluster (..),
      showClusters,
      extractUpdatedPixelList,
    ) where

import ImageDefinition.Pixel (showPixels, Pixel (..))
import ImageDefinition.Color (Color)

type NewAverageColor = Color
type OldAverageColor = Color

data Cluster = Cluster NewAverageColor OldAverageColor [Pixel]

showClusters :: [Cluster] -> [String]
showClusters []                     = []
showClusters (Cluster col _ pix:xs) = showClusters xs ++ ["--\n" ++ show col ++ "\n-"] ++ showPixels pix

extractUpdatedPixelList :: [Cluster] -> [Pixel]
extractUpdatedPixelList = concatMap extractUpdatedPixelList'

{-# INLINE extractUpdatedPixelList' #-}
extractUpdatedPixelList' :: Cluster -> [Pixel]
extractUpdatedPixelList' (Cluster col _ pix) = extractUpdatedPixelList'' col pix

{-# INLINE extractUpdatedPixelList'' #-}
extractUpdatedPixelList'' :: Color -> [Pixel] -> [Pixel]
extractUpdatedPixelList'' _ []                   = []
extractUpdatedPixelList'' col ((Pixel pos _):ix) = Pixel pos col : extractUpdatedPixelList'' col ix