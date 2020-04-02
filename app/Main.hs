module Main where

import Lib
import System.Exit

main :: IO ()
main = exitWith $ ExitFailure 84
