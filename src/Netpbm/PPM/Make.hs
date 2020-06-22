module Netpbm.PPM.Make
  ( PpmBlueprint,
    newPpmBlueprint,
    Serializable (..),
    Craftable (..),
  ) where

import Data.ByteString (ByteString, pack, append, writeFile)
import Prelude hiding (writeFile)
import Data.Word (Word8)
import qualified Data.Text as TXT (pack)
import Data.Text.Encoding (encodeUtf8)

import Netpbm.PPM.Definition (PpmTranslatable (..))
import Netpbm.Info (MagicNumber (..))
import ImageDefinition.Cluster (Cluster (..), extractUpdatedPixelList)
import ImageDefinition.Pixel (Pixel (..), sortPixels)
import ImageDefinition.Position (Position (..), X (..), Y (..), newPosition)
import ImageDefinition.Color (Color (..))

type Octet = Word8
type FileName = String

class Serializable a where
    toBytestring :: a -> ByteString

class Serializable a => Craftable a where
    craft :: a -> IO ()

newtype Width = Width String
newtype Height = Height String
newtype MaxValue = MaxValue String
newtype RgbContent = RgbContent [Octet]
instance  Serializable RgbContent where
    toBytestring (RgbContent c) = pack c

data PpmHeader = PpmHeader MagicNumber Width Height MaxValue
instance Serializable PpmHeader where
    toBytestring (PpmHeader num (Width w) (Height h) (MaxValue mv)) = sToBs $ show num ++ "\n" ++ h ++ " " ++ w ++ "\n" ++ mv ++ "\n"

data PpmBlueprint = PpmBlueprint FileName PpmHeader RgbContent
instance Serializable PpmBlueprint where
    toBytestring (PpmBlueprint _ head rgb) = toBytestring head `append` toBytestring rgb
instance Craftable PpmBlueprint where
    craft bp@(PpmBlueprint name _ _) = writeFile name $ toBytestring bp

{-# INLINE sToBs #-}
sToBs :: String -> ByteString
sToBs = encodeUtf8 . TXT.pack

newPpmBlueprint :: FileName -> [Cluster] -> PpmBlueprint
newPpmBlueprint s pix = newPpmBlueprint' s $ sortPixels $ extractUpdatedPixelList pix

{-# INLINE newPpmBlueprint' #-}
newPpmBlueprint' :: FileName -> [Pixel] -> PpmBlueprint
newPpmBlueprint' s pix = newPpmBlueprint'' s (getDimension pix) $ colorsToRgbContent $ map (\(Pixel _ col) -> col) pix

{-# INLINE newPpmBlueprint'' #-}
newPpmBlueprint'' :: FileName -> (Width, Height) -> RgbContent -> PpmBlueprint
newPpmBlueprint'' s (w, h) = PpmBlueprint s (PpmHeader P6 w h (MaxValue "255"))

colorsToRgbContent :: [Color] -> RgbContent
colorsToRgbContent = RgbContent . colorsToRgbContent'

{-# INLINE colorsToRgbContent' #-}
colorsToRgbContent' :: [Color] -> [Octet]
colorsToRgbContent' [] = []
colorsToRgbContent' ((Color r g b):xs) = toOctet r : toOctet g : toOctet b : colorsToRgbContent' xs

getDimension :: [Pixel] -> (Width, Height)
getDimension = makeDimensionTuple . foldl keepWider (newPosition 0 0)

{-# INLINE makeDimensionTuple #-}
makeDimensionTuple :: Position -> (Width, Height)
makeDimensionTuple (Position (X x) (Y y)) = (Width $ show (x + 1), Height $ show (y + 1))

keepWider :: Position -> Pixel -> Position
keepWider pos@(Position x y) (Pixel (Position x1 y1) _) | x1 > x && y1 > y = Position x1 y1
                                                        | x1 > x           = Position x1 y
                                                        | y1 > y           = Position x  y1
                                                        | otherwise        = pos