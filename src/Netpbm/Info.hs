module Netpbm.Info
  ( MagicNumber (..),
  ) where

data MagicNumber = P1 | P2 | P3 | P4 | P5 | P6

instance Show MagicNumber where
    show P1 = "P1"
    show P2 = "P2"
    show P3 = "P3"
    show P4 = "P4"
    show P5 = "P5"
    show P6 = "P6"