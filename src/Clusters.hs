--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- Clusters
--

module Clusters
    ( Cluster(..)
    , printClusters
    )
where

import           DataTypes

data Cluster = Cluster {
    clColor :: Color,
    clPixels :: [Pixel]
}

printClusters :: [Cluster] -> IO ()
printClusters = mapM_ printCluster

printCluster :: Cluster -> IO ()
printCluster cluster = do
    putStrLn "--"
    printColor (clColor cluster)
    putStrLn ""
    putStrLn "-"
    mapM_ printPixelLn (clPixels cluster)
    where printPixelLn pixel = printPixel pixel >> putStrLn ""
