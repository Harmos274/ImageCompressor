module ImageDefinition.Position
    (Position (..),
     newPosition,
     X (..),
     Y (..),
    ) where

newtype X     = X Int deriving(Eq, Ord)
newtype Y     = Y Int deriving(Eq, Ord)

data Position = Position X Y deriving(Eq)

instance Show Position where
    show (Position (X x) (Y y)) = '(' : show x ++ ',' : show y ++ ")"

newPosition :: Int -> Int -> Position
newPosition x y = Position (X x) (Y y)

