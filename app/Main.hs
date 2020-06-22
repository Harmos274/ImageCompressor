module Main where

-- System
import Control.Exception (throw, handle, catch, IOException)
import System.Environment (getArgs)
--

import ArgumentManager (Argument (..), getPath, parseAndLexArguments)
import ImageDefinition.Cluster (Cluster)
import ImageDefinition.Pixel (Pixel)
import Compressor (compressor, initClusters)
import FileReader (parseFile)
import Exception (exceptionHandler, ICExceptions(..))
import Netpbm.PPM.Make (craft, newPpmBlueprint)

main :: IO ()
main = handle exceptionHandler $
       do argv <- getArgs
          let args = parseAndLexArguments argv
          fileContent <- catch (readFile $ getPath args) (\e -> throw $ BadArgument $ show (e::IOException))
          craft $ newPpmBlueprint "toto.ppm" $ imageCompressor args $ parseFile fileContent

imageCompressor :: Argument -> [Pixel] -> [Cluster]
imageCompressor (Argument col conv _) pix = compressor (initClusters pix col) pix conv
