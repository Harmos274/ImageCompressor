module Exception
    (   ICExceptions (..),
        exceptionHandler
    ) where

import Control.Exception (throw, Exception)
import Epitech.ReturnType

data ICExceptions = SendHelp
                | BadArgument String
                | RuntimeException String
                deriving (Show)
instance Exception ICExceptions

sendHelp :: IO ()
sendHelp = mapM_ putStrLn ["USAGE: ./imageCompressor n e IN\n",
                           "\tn\tnumber of colors in the final image",
                           "\te\tconvergence limit",
                           "\tIN\tpath to the file containing the colors of the pixels"]

exceptionHandler :: ICExceptions -> IO ()
exceptionHandler SendHelp               = sendHelp >> success
exceptionHandler (BadArgument s)        = putStrLn ("Bad argument : " ++ s) >> failure
exceptionHandler (RuntimeException s)   = putStrLn ("Runtime exception : " ++ s) >> failure
