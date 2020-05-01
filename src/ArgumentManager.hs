module ArgumentManager
    ( getColorLimit,
      getConvergenceLimit,
      getPath,
      lexer,
      parser,
      Argument,
      ColorLimit,
      ConvergenceLimit,
      Path
    ) where

import Text.Read (readMaybe)
import Control.Exception (throw)

import Exception (ICExceptions (BadArgument, SendHelp))

type ColorLimit = Int
type ConvergenceLimit = Int
type Path = String

data Argument = Argument ColorLimit ConvergenceLimit Path deriving(Show)
data Token = Limit Int | Path String | HELP


readLimit :: Maybe Int -> Token
readLimit (Just a) = Limit a
readLimit Nothing  = throw $ BadArgument "Limits must be integer."

lexer :: [String] -> [Token]
lexer []        = []
lexer ("-h":xs) = [HELP]
lexer [l]       = [Path l]
lexer (l:xs)    = readLimit (readMaybe l) : lexer xs

parser :: [Token] -> Argument
parser (HELP:_)                        = throw SendHelp
parser [Limit col, Limit conv, Path l] = Argument col conv l
parser _                               = throw $ BadArgument "Bad usage, retry with -h."

getColorLimit :: Argument -> ColorLimit
getColorLimit (Argument col _ _) = col

getConvergenceLimit :: Argument -> ConvergenceLimit
getConvergenceLimit (Argument _ conv _) = conv

getPath :: Argument -> Path
getPath (Argument _ _ path) = path

