module ImageDefinition
    (Position (..),
     newPosition,
     Color (..),
     newColor,
     Pixel (..),
     showColor,
     showPosition,
     showPixels,
     newPixel,
     X (..),
     Y (..),
     R,
     G,
     B,
    ) where

import Control.Exception (throw)

import Exception (ICExceptions (RuntimeException))

newtype X     = X Int
newtype Y     = Y Int

data Position = Position X Y

showPosition :: Position -> String
showPosition (Position (X x) (Y y)) = "(" ++ show x ++ "," ++ show y ++ ")"

newPosition :: Int -> Int -> Position
newPosition x y = Position (X x) (Y y)

type R = Float
type G = Float
type B = Float

data Color c = Color c c c deriving(Eq, Show)
instance Num a => Num (Color a) where
    Color r1 g1 b1 + Color r2 g2 b2 = Color (r1 + r2) (g1 + g2) (b1 + b2)
    Color r1 g1 b1 * Color r2 g2 b2 = Color (r1 * r2) (g1 * g2) (b1 * b2)
    Color r1 g1 b1 - Color r2 g2 b2 = Color (r1 - r2) (g1 - g2) (b1 - b2)
    abs (Color r g b)       = Color (abs r) (abs g) (abs b)
    signum (Color r g b)    = Color (signum r) (signum g) (signum b)
    fromInteger             = throw $ RuntimeException "Can't use fromInteger with a Color data type."

showColor :: Color Float -> String
showColor (Color r g b) = "("++ show (round r) ++ "," ++ show (round g) ++ "," ++ show (round b) ++ ")"

newColor :: R -> G -> B -> Color Float
newColor = Color

data Pixel = Pixel Position (Color Float)

showPixels :: [Pixel] -> [String]
showPixels [] = []
showPixels (Pixel pos col:xs) = (showPosition pos ++ " " ++ showColor col) : showPixels xs

newPixel :: (Position, Color Float) -> Pixel
newPixel (p, c) = Pixel p c
