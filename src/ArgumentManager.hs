module ArgumentManager
    ( getColorLimit,
      getConvergenceLimit,
      getPath,
      lexer,
      parser,
      Argument (..),
      ColorLimit (..),
      ConvergenceLimit (..),
      Path (..)
    ) where

import Text.Read (readMaybe)
import Control.Exception (throw)

import Exception (ICExceptions (BadArgument, SendHelp))

newtype ColorLimit = ColorLimit Int deriving(Show)
newtype ConvergenceLimit = ConvergenceLimit Float deriving(Show)
newtype Path = FilePath String deriving(Show)

data Token = Limit String | Path String | HELP

data Argument = Argument ColorLimit ConvergenceLimit Path deriving(Show)

newArgument :: ColorLimit -> ConvergenceLimit -> String -> Argument
newArgument col conv l = Argument col conv (FilePath l)

readConvergenceLimit :: Maybe Float -> ConvergenceLimit
readConvergenceLimit (Just a) | a > 0     = ConvergenceLimit a
                              | otherwise = throw $ BadArgument "Convergence limit must be positive."
readConvergenceLimitNothing               = throw $ BadArgument "Convergence limit must be integer."

readColorLimit :: Maybe Int -> ColorLimit
readColorLimit (Just a) | a > 0     = ColorLimit a
                        | otherwise = throw $ BadArgument "Color limit must be positive."
readColorLimit Nothing              = throw $ BadArgument "Color limit must be integer."

lexer :: [String] -> [Token]
lexer []        = []
lexer ("-h":xs) = [HELP]
lexer [l]       = [Path l]
lexer (l:xs)    = Limit l : lexer xs

parser :: [Token] -> Argument
parser (HELP:_)                        = throw SendHelp
parser [Limit col, Limit conv, Path l] = newArgument (readColorLimit $ readMaybe col) (readConvergenceLimit $ readMaybe conv) l
parser _                               = throw $ BadArgument "Bad usage, retry with -h."

getColorLimit :: Argument -> ColorLimit
getColorLimit (Argument col _ _) = col

getConvergenceLimit :: Argument -> ConvergenceLimit
getConvergenceLimit (Argument _ conv _) = conv

getPath :: Argument -> String
getPath (Argument _ _ (FilePath path)) = path
