module ArgumentTest
    ( lexerTest,
      parserTest
    ) where

import Control.Exception (evaluate)
import Test.Tasty
import Test.Tasty.HUnit

import Exception (ICExceptions (SendHelp, BadArgument))
import Assert (assertICException)
import ArgumentManager (lexer, parser, Token (..), Argument (..), ColorLimit (..), ConvergenceLimit (..), Path (..))

newtype TestToken = TestToken Token

instance Show TestToken where
    show (TestToken (Limit str)) = str
    show (TestToken (Path str))  = "./" ++ str
    show (TestToken HELP)        = "HELP"

instance Eq TestToken where
    (TestToken (Limit str)) == (TestToken (Limit str2)) = str == str2
    (TestToken (Path str))  == (TestToken (Path str2))  = str == str2
    (TestToken HELP)        == (TestToken HELP)         = True
    _                       == _                        = False

newtype TestArgument = TestArgument Argument

instance Show TestArgument where
    show (TestArgument (Argument (ColorLimit cl) (ConvergenceLimit l) (FilePath str))) =
        "ColorLimit" ++ show cl ++ "ConvergenceLimit" ++ show l ++ "Path" ++ str

instance Eq TestArgument where
    (TestArgument (Argument (ColorLimit lim) (ConvergenceLimit clim) (FilePath str))) ==
        (TestArgument (Argument (ColorLimit lim1) (ConvergenceLimit clim1) (FilePath str1))) =
            lim == lim1 && clim == clim1 && str == str1

lexed :: [Token]
lexed = [Limit "222", Limit "124", Path "path"]

invLexed :: [Token]
invLexed = [Limit "222", Limit "223"]

lexerTest :: TestTree
lexerTest = testGroup "Lexer Test" [helpLexerTest, completeLexerTest]

helpLexerTest :: TestTree
helpLexerTest = testCase "Valid -h input" $ assertEqual "Invalid output token on argument lexer."
    (map TestToken (lexer ["-h", "124", "path"])) [TestToken HELP]

completeLexerTest :: TestTree
completeLexerTest = testCase "Complete argument input" $ assertEqual "Invalid output on arguments"
    (map TestToken (lexer ["222", "124", "path"])) (map TestToken lexed)

parserTest :: TestTree
parserTest = testGroup "Parser Test" [helpParserTest, completeParserTest, invalidParserTest]

helpParserTest :: TestTree
helpParserTest = testCase "HELP token throw" $ assertICException SendHelp (evaluate $ parser [HELP])

completeParserTest :: TestTree
completeParserTest = testCase "Complete argument token" $ assertEqual "Invalid argument data"
    (TestArgument $ parser [Limit "122", Limit "124", Path "path"])
        (TestArgument $ Argument (ColorLimit 122) (ConvergenceLimit 124) (FilePath "path"))

invalidParserTest :: TestTree
invalidParserTest = testCase "Invalid argument token" $
        assertICException (BadArgument "Bad usage, retry with -h.") (evaluate $ parser invLexed)
