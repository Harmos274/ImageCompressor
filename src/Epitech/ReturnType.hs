module Epitech.ReturnType
    ( success,
      failure
    ) where

import System.Exit (exitSuccess, ExitCode (ExitFailure), exitWith)

success :: IO ()
success = exitSuccess

failure :: IO ()
failure = exitWith $ ExitFailure 84
