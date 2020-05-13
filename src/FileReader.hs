module FileReader
    (parseFile
    ) where

import Control.Exception (throw)
import Text.Read (readMaybe)
import System.IO (readFile)
import Data.Char (isDigit)
import Control.Arrow (second)

import Exception (ICExceptions(FileReaderException))
import ImageDefinition (Pixel, newPixel, Color, newColor, Position, newPosition)

data Token = OpenParen | CloseParen | Comma | Space | Value String

parseFile :: String -> [Pixel]
parseFile str = map (parser . lexer) $ lines str

parser :: [Token] -> Pixel
parser = newPixel . second parseColor . parsePosition

parseColor :: [Token] -> Color Float
parseColor [Space, OpenParen, r, Comma, g, Comma, b, CloseParen] = newColor (readColor r) (readColor g) (readColor b)
parseColor _ = throw FileReaderException

parsePosition :: [Token] -> (Position, [Token])
parsePosition (OpenParen : x : Comma : y : CloseParen : xs) = (newPosition (readValue x) (readValue y), xs)
parsePosition _ =  throw FileReaderException

lexer :: String -> [Token]
lexer [] = []
lexer ('(':xs) = OpenParen : lexer xs
lexer (')':xs) = CloseParen : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer (' ':xs) = Space : lexer xs
lexer str      = Value nbr : lexer xs
    where (nbr, xs) = getDigits str

getDigits :: String -> (String, String)
getDigits = span isDigit

readColor :: Token -> Float
readColor (Value v) = readColor' (readMaybe v)
readColor _         = throw FileReaderException

readColor' :: Maybe Float -> Float
readColor' (Just value) | value >= 0 && value <= 255 = value
                        | otherwise = throw FileReaderException
readColor' Nothing = throw FileReaderException


readValue :: Token -> Int
readValue (Value v) = readValue' (readMaybe v)
readValue _         = throw FileReaderException

readValue' :: Maybe Int -> Int
readValue' (Just value) | value >= 0 && value <= 255 = value
                        | otherwise = throw FileReaderException
readValue' Nothing = throw FileReaderException

