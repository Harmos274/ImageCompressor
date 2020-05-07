module ImageDefinition (Position,
                        Color,
                        Pixel,
                        Cluster
                       ) where

type X = Int
type Y = Int
type Position = (X, Y)

type R = Int
type G = Int
type B = Int
type Color = (R, G, B)

type Pixel = (Position, Color)

type Cluster = (Color, [Pixel])
