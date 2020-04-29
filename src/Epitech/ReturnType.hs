module Epitech.ReturnType
    ( success,
      failure
    ) where

import System.Exit

success :: IO ()
success = exitSuccess

failure :: IO ()
failure = exitWith $ ExitFailure 84
