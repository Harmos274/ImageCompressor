{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ArgumentManager
    ( getColorLimit,
      getConvergenceLimit,
      parseAndLexArguments,
      getPath,
      lexer,
      parser,
      Argument (..),
      ColorLimit (..),
      ConvergenceLimit (..),
      Path (..)
    ) where

import Exception (ICExceptions (BadArgument, SendHelp))

import Text.Read (readMaybe)
import Control.Exception (throw)

newtype ColorLimit = ColorLimit Int deriving(Num)
newtype ConvergenceLimit = ConvergenceLimit Float
newtype Path = FilePath String

data Token = Limit String | Path String | HELP

data Argument = Argument ColorLimit ConvergenceLimit Path

newArgument :: ColorLimit -> ConvergenceLimit -> String -> Argument
newArgument col conv l = Argument col conv (FilePath l)

parseAndLexArguments :: [String] -> Argument
parseAndLexArguments = parser . lexer

lexer :: [String] -> [Token]
lexer []       = []
lexer ("-h":_) = [HELP]
lexer [l]      = [Path l]
lexer (l:xs)   = Limit l : lexer xs

parser :: [Token] -> Argument
parser (HELP:_)                        = throw SendHelp
parser [Limit col, Limit conv, Path l] = newArgument (readColorLimit $ readMaybe col) (readConvergenceLimit $ readMaybe conv) l
parser _                               = throw $ BadArgument "Bad usage, retry with -h."

readColorLimit :: Maybe Int -> ColorLimit
readColorLimit (Just a) | a > 0 = ColorLimit a
readColorLimit _                = throw $ BadArgument "Color limit must be a positive integer."

readConvergenceLimit :: Maybe Float -> ConvergenceLimit
readConvergenceLimit (Just a) | a > 0 = ConvergenceLimit a
readConvergenceLimit _                = throw $ BadArgument "Convergence limit must be a positive float."

getColorLimit :: Argument -> ColorLimit
getColorLimit (Argument col _ _) = col

getConvergenceLimit :: Argument -> ConvergenceLimit
getConvergenceLimit (Argument _ conv _) = conv

getPath :: Argument -> String
getPath (Argument _ _ (FilePath path)) = path
