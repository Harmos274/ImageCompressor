module FileReader
    ( parseFile,
      parser,
      lexer,
      Token(..),
      readValue,
      readColor,
    ) where

import Control.Exception(throw)
import Text.Read(readMaybe)
import Data.Char(isDigit)
import Control.Arrow(second)

import Exception (ICExceptions(FileReaderException))
import ImageDefinition.Pixel (Pixel, newPixel)
import ImageDefinition.Color (Color, newColor)
import ImageDefinition.Position (Position, newPosition)

data Token = OpenParen | CloseParen | Comma | Space | Value String

parseFile :: String -> [Pixel]
parseFile str = map (parser . lexer) $ lines str

parser :: [Token] -> Pixel
parser [] = throw FileReaderException
parser t  = (newPixel . second parseColor . parsePosition) t

parseColor :: [Token] -> Color
parseColor [Space, OpenParen, r, Comma, g, Comma, b, CloseParen] = newColor (readColor r) (readColor g) (readColor b)
parseColor _                                                     = throw FileReaderException

parsePosition :: [Token] -> (Position, [Token])
parsePosition (OpenParen : x : Comma : y : CloseParen : xs) = (newPosition (readValue x) (readValue y), xs)
parsePosition _                                             =  throw FileReaderException

lexer :: String -> [Token]
lexer []       = []
lexer ('(':xs) = OpenParen : lexer xs
lexer (')':xs) = CloseParen : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer (' ':xs) = Space : lexer xs
lexer str      = (lexer' . getDigits) str

{-# INLINE lexer' #-}
lexer' :: (String, String) -> [Token]
lexer' (nbr, xs) = Value nbr : lexer xs

getDigits :: String -> (String, String)
getDigits = span isDigit

readColor :: Token -> Float
readColor (Value v) = readColor' (readMaybe v)
readColor _         = throw FileReaderException

{-# INLINE readColor' #-}
readColor' :: Maybe Float -> Float
readColor' (Just value) | value >= 0 && value <= 255 = value
readColor' _                                         = throw FileReaderException

readValue :: Token -> Int
readValue (Value v) = readValue' (readMaybe v)
readValue _         = throw FileReaderException

{-# INLINE readValue' #-}
readValue' :: Maybe Int -> Int
readValue' (Just value) | value >= 0 && value <= 255 = value
readValue' _                                         = throw FileReaderException
