module CompressorTest
    ( convergenceTest,
      clusterTest,
      algorithmTest
    ) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Control.Exception (evaluate)

import Exception (ICExceptions (RuntimeException))
import Assert (assertICException)
import Compressor (euclideanDistance, hasConverged, fillCluster, clusterMean, initClusters)
import ArgumentManager (ConvergenceLimit (..), ColorLimit (..))
import ImageDefinition.Color
import ImageDefinition.Cluster
import ImageDefinition.Pixel
import ImageDefinition.Position

newtype TestCluster = TestCluster Cluster

instance Show TestCluster where
    show (TestCluster (Cluster color1 color2 pixels)) = show color1 ++ show color2 ++ (unlines . showPixels) pixels

instance Eq TestCluster where
    (TestCluster (Cluster color1 color2 pixels)) == (TestCluster (Cluster color3 color4 pixels2)) =
        color1 == color3 && color2 == color4 && pixels == pixels2

convergenceTest :: TestTree
convergenceTest = testGroup "Convergence Test" [euclideanDistanceTest, convergedTest, convergedntTest]

euclideanDistanceTest :: TestTree
euclideanDistanceTest = testCase "Compute Euclidean distance" $ assertEqual "Invalid euclidean distance"
    (euclideanDistance (Color 0 0 0) (Color 12 12 12)) (20.784609 :: Float)

convergedTest :: TestTree
convergedTest = testCase "Has converged" $ assertBool "Has not converged" $
    hasConverged (ConvergenceLimit 1.0) [Cluster (Color 0 0 0) (Color 0 0 0) []]

convergedntTest :: TestTree
convergedntTest = testCase "Has convergedn't" $ assertBool "Has converged" $
    not $ hasConverged (ConvergenceLimit 1.0) [Cluster (Color 0 0 0) (Color 10 10 10) []]

clusterTest :: TestTree
clusterTest = testGroup "Cluster Test" [initClusterTest, showClusterTest]

initClusterTest :: TestTree
initClusterTest = testGroup "Init Cluster Test" [noDoubleColorInitClusterTest, doubleColorInitClusterTest,
                                                 notEnoughColorInitClusterTest]

pixelList :: [Pixel]
pixelList = [Pixel (newPosition 0 0) (Color 0 0 0), Pixel (newPosition 0 0) (Color 1 1 1), Pixel (newPosition 0 0) (Color 2 2 2),
             Pixel (newPosition 0 0) (Color 3 3 3)]

clusterList :: [Cluster]
clusterList = [Cluster (Color 0 0 0) (Color 0 0 0) [head pixelList], Cluster (Color 1 1 1) (Color 1 1 1) [pixelList!!1],
               Cluster (Color 2 2 2) (Color 2 2 2) [pixelList!!2], Cluster (Color 3 3 3) (Color 3 3 3) [pixelList!!3]]

emptyClusterList :: [Cluster]
emptyClusterList = [Cluster (Color 0 0 0) (Color 0 0 0) [], Cluster (Color 1 1 1) (Color 1 1 1) [],
                    Cluster (Color 2 2 2) (Color 2 2 2) [], Cluster (Color 3 3 3) (Color 3 3 3) []]

noDoubleColorInitClusterTest :: TestTree
noDoubleColorInitClusterTest = testCase "No double color" $ assertEqual "Invalid cluster"
    (map TestCluster $ initClusters pixelList (ColorLimit 4)) (map TestCluster $ reverse clusterList)

doubleColorInitClusterTest :: TestTree
doubleColorInitClusterTest = testCase "Double color" $ assertEqual "Invalid cluster"
    (map TestCluster $ initClusters (Pixel (newPosition 0 0) (Color 0 0 0):pixelList) (ColorLimit 4)) (map TestCluster $ reverse clusterList)

notEnoughColorInitClusterTest :: TestTree
notEnoughColorInitClusterTest = testCase "Not enough color" $ assertICException (RuntimeException "Not enough different colors to make clusters.")
    (evaluate $ initClusters [] (ColorLimit 2))

showClusterTest :: TestTree
showClusterTest = testGroup "Show Cluster" [notEmptyShowClusterTest, emptyShowClusterTest]

notEmptyShowClusterTest :: TestTree
notEmptyShowClusterTest = testCase "Not empty cluster list" $ assertEqual "Invalid clusters"
    (showClusters clusterList) ["--\n(3,3,3)\n-", "(0,0) (3,3,3)", "--\n(2,2,2)\n-", "(0,0) (2,2,2)",
                                "--\n(1,1,1)\n-", "(0,0) (1,1,1)", "--\n(0,0,0)\n-", "(0,0) (0,0,0)"]

emptyShowClusterTest :: TestTree
emptyShowClusterTest = testCase "Empty cluster list" $ assertEqual "Invalid clusters" (showClusters []) []

algorithmTest :: TestTree
algorithmTest = testGroup "Algorithm Test" [fillClusterTest, clusterMeanTest]

fillClusterTest :: TestTree
fillClusterTest = testCase "Fill cluster Test" $ assertEqual "invalid cluster filling"
    (map TestCluster $ fillCluster pixelList emptyClusterList) (map TestCluster $ reverse clusterList)

clusterMeanTest :: TestTree
clusterMeanTest = testCase "Cluster Mean Test" $ assertEqual "invalid cluster filling"
    (map TestCluster $ clusterMean clusterList) (map TestCluster emptyClusterList)
