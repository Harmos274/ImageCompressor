module ImageDefinition.Pixel
    ( Pixel (..),
      newPixel,
      showPixels,
    ) where

import ImageDefinition.Position (Position)
import ImageDefinition.Color (Color)

data Pixel = Pixel Position Color deriving Eq
instance Show Pixel where
    show (Pixel pos col) = show pos ++ ' ' : show col

newPixel :: (Position, Color) -> Pixel
newPixel (p, c) = Pixel p c

showPixels :: [Pixel] -> [String]
showPixels = map show
