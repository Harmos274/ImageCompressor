module Assert
    ( assertICException
    ) where

import Test.HUnit (assertFailure, Assertion)
import Exception (ICExceptions (..))
import Control.Exception (handleJust)
import Control.Monad (return, guard)

newtype TestException = TestException ICExceptions deriving(Show)

instance Eq TestException where
    (TestException SendHelp)             == (TestException SendHelp)             = True
    (TestException FileReaderException)  == (TestException FileReaderException)  = True
    (TestException (BadArgument _))      == (TestException (BadArgument _))      = True
    (TestException (RuntimeException _)) == (TestException (RuntimeException _)) = True
    _                                    == _                                    = False

assertICException :: ICExceptions -> IO a -> Assertion
assertICException ex action = handleJust handler (const $ return ()) $
        do  _ <- action
            assertFailure $ "Expected exception: " ++ show ex
        where handler :: ICExceptions -> Maybe ()
              handler e = guard (TestException e == TestException ex)
