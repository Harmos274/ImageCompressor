module FileReaderTest
    ( lexerTest,
      parserTest,
      readColorTest,
      readValueTest,
    ) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertEqual)
import Control.Exception (evaluate)

import Assert (assertICException)
import FileReader (parser, Token (..), lexer, readColor, readValue)
import ImageDefinition.Pixel
import ImageDefinition.Color
import ImageDefinition.Position
import Exception (ICExceptions(FileReaderException))

newtype TestToken = TestToken Token

instance Show TestToken where
    show (TestToken OpenParen)   = "("
    show (TestToken Comma)       = ","
    show (TestToken CloseParen)  = ")"
    show (TestToken (Value str)) = str
    show (TestToken Space)       = " "

instance Eq TestToken where
    (TestToken OpenParen)   == (TestToken OpenParen)    = True
    (TestToken CloseParen)  == (TestToken CloseParen)   = True
    (TestToken Comma)       == (TestToken Comma)        = True
    (TestToken Space)       == (TestToken Space)        = True
    (TestToken (Value str)) == (TestToken (Value str2)) = str == str2
    _                       == _                        = False

lexed :: [Token]
lexed = [OpenParen, Value "0", Comma, Value "0", CloseParen,
         Space,
         OpenParen, Value "128", Comma, Value "128", Comma, Value "128", CloseParen]

lexerTest :: TestTree
lexerTest = testGroup "Lexer Test" [validLexerTest]

validLexerTest :: TestTree
validLexerTest = testCase "Valid input" $ assertEqual "Invalid output token on file lexer."
    (map TestToken (lexer "(0,0) (128,128,128)")) (map TestToken lexed)

parserTest :: TestTree
parserTest = testGroup "Parser Test" [invalidParserTest, validParserTest]

invalidParserTest :: TestTree
invalidParserTest = testCase "Invalid token list" $ assertICException FileReaderException (evaluate $ parser [])

validParserTest :: TestTree
validParserTest = testCase "Valid token list" $ assertEqual "Invalid color parsing from token."
    (parser lexed) (Pixel (Position (X 0) (Y 0)) $ Color (R 128.0) (G 128.0) (B 128.0))

readValueTest :: TestTree
readValueTest = testCase "Read value Test" $ assertICException FileReaderException (evaluate $ readValue (Value "256"))

readColorTest :: TestTree
readColorTest = testCase "Read color Test" $ assertICException FileReaderException (evaluate $ readColor (Value "256"))


