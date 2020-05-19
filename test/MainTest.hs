import Test.Tasty

import qualified FileReaderTest (lexerTest, parserTest, readColorTest, readValueTest)
import qualified CompressorTest (convergenceTest, clusterTest, algorithmTest)
import qualified ArgumentTest   (lexerTest, parserTest)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ImageCompressor Tests" [argumentT, fileReaderT, compressorT]

argumentT :: TestTree
argumentT = testGroup "Arguments" [ArgumentTest.lexerTest, ArgumentTest.parserTest]

fileReaderT :: TestTree
fileReaderT = testGroup "FileReader" [FileReaderTest.lexerTest, FileReaderTest.parserTest,
                                      FileReaderTest.readColorTest, FileReaderTest.readValueTest]

compressorT :: TestTree
compressorT = testGroup "Compressor" [CompressorTest.convergenceTest, CompressorTest.clusterTest, CompressorTest.algorithmTest]
