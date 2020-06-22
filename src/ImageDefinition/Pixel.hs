module ImageDefinition.Pixel
    ( Pixel (..),
      newPixel,
      showPixels,
      sortPixels,
    ) where

import Data.List (sortOn)

import ImageDefinition.Position (Position (..), X (..), Y (..))
import ImageDefinition.Color (Color)

data Pixel = Pixel Position Color deriving Eq
instance Show Pixel where
    show (Pixel pos col) = show pos ++ ' ' : show col

newPixel :: (Position, Color) -> Pixel
newPixel (p, c) = Pixel p c

showPixels :: [Pixel] -> [String]
showPixels = map show

sortPixels :: [Pixel] -> [Pixel]
sortPixels pix = sortOn (genPixelId $ length pix) pix

{-# INLINE genPixelId #-}
genPixelId :: Int -> Pixel -> Int
genPixelId len (Pixel (Position (X x) (Y y)) _) = x + y * len