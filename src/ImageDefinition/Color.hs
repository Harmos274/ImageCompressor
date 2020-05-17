{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ImageDefinition.Color
    ( Color (..),
      FloatFractionable (..),
      newColor,
      R (..),
      G (..),
      B (..),
    ) where

import Control.Exception (throw)
import Exception (ICExceptions (RuntimeException))

class FloatFractionable a where
    divide :: Float -> a -> a

newtype R = R Float deriving(Num, Eq, Fractional, Ord, Real, RealFrac)
instance FloatFractionable R where
    divide i (R a) = R (a / i)

newtype G = G Float deriving(Num, Eq, Fractional, Ord, Real, RealFrac)
instance FloatFractionable G where
    divide i (G a) = G (a / i)

newtype B = B Float deriving(Num, Eq, Fractional, Ord, Real, RealFrac)
instance FloatFractionable B where
    divide i (B a) = B (a / i)

type Rf = Float
type Gf = Float
type Bf = Float

data Color = Color R G B deriving(Eq)
instance Num Color where
    Color r1 g1 b1 + Color r2 g2 b2 = Color (r1 + r2) (g1 + g2) (b1 + b2)
    Color r1 g1 b1 * Color r2 g2 b2 = Color (r1 * r2) (g1 * g2) (b1 * b2)
    Color r1 g1 b1 - Color r2 g2 b2 = Color (r1 - r2) (g1 - g2) (b1 - b2)
    abs (Color r g b)               = Color (abs r) (abs g) (abs b)
    signum (Color r g b)            = Color (signum r) (signum g) (signum b)
    fromInteger                     = throw $ RuntimeException "Can't use fromInteger with a Color data type."
instance Show Color where
    show (Color r g b) = '(' : show (round r) ++ ',' : show (round g) ++ ',' : show (round b) ++ ")"

newColor :: Rf -> Gf -> Bf -> Color
newColor r g b = Color (R r) (G g) (B b)
