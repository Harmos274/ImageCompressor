module Main where

-- System
import Control.Exception (throw, handle)
import System.Environment (getArgs)
--

import Exception (ICExceptions (SendHelp), exceptionHandler)

main :: IO ()
main = handle exceptionHandler $ getArgs >>= imageCompressor

-- to change, potential final return type will be [String]
imageCompressor :: [String] -> IO ()
imageCompressor _ = throw SendHelp
