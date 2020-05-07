module FileReader (parseFile) where

import Control.Exception (throw)
import Text.Read (readMaybe)
import System.IO (readFile)
import Data.Char (isDigit)

import Exception (ICExceptions(FileReaderException))
import ImageDefinition (Pixel)

data Token = OpenParen | CloseParen | Comma | Space | Value Int

getDigits :: String -> (String, String)
getDigits = span isDigit

readValue :: Maybe Int -> Token
readValue (Just value) | value >= 0 && value <= 255 = Value value
                       | otherwise = throw $ FileReaderException
readValue Nothing = throw $ FileReaderException

lexer :: String -> [Token]
lexer [] = []
lexer ('(':xs) = OpenParen : lexer xs
lexer (')':xs) = CloseParen : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer (' ':xs) = Space : lexer xs
lexer str = readValue (readMaybe nbr) : lexer xs
    where (nbr, xs) = getDigits str

parser :: [Token] -> Pixel
parser [OpenParen, (Value posX), Comma, (Value posY), CloseParen, Space, OpenParen, (Value colorR), Comma, (Value colorG), Comma, (Value colorB), CloseParen] = ((posX, posY), (colorR, colorG, colorB))
parser _ = throw $ FileReaderException

getNextLine :: String -> (String, String)
getNextLine str = span (/='\n') str

splitString :: String -> [String]
splitString str | line /= "" = line : splitString (drop 1 xs)
                | otherwise = []
    where (line, xs) = getNextLine str

parseFile :: String -> [Pixel]
parseFile str = map (parser . lexer) $ splitString str
