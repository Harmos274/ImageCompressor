module Main where

-- System
import Control.Exception (throw, handle, catch, IOException)
import System.Environment (getArgs)
--

import ArgumentManager
import ImageDefinition (Pixel(..))
import Compressor
import FileReader
import Exception (exceptionHandler, ICExceptions(..))

main :: IO ()
main = handle exceptionHandler $
       do argv <- getArgs
          let args = parseAndLexArguments argv
          fileContent <- catch (readFile $ getPath args) (\e -> throw $ BadArgument $ show (e::IOException))
          mapM_ putStrLn (showCluster $ imageCompressor args $ parseFile fileContent)


parseAndLexArguments :: [String] -> Argument
parseAndLexArguments l = parser $ lexer l

imageCompressor :: Argument -> [Pixel] -> [Cluster]
imageCompressor (Argument col conv (FilePath path)) pix = compressor (initClusters pix col) pix conv
