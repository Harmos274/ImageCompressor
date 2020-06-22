module Netpbm.PPM.Definition
  ( PpmTranslatable (..),
  ) where

import Data.Word (Word8 (..))
type Octet = Word8

class RealFrac a => PpmTranslatable a where
    toOctet :: a -> Octet
    toOctet = toEnum . round