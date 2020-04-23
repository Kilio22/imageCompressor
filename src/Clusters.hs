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

import           DataTypes                      ( Color
                                                , Pixel
                                                )

data Cluster = Cluster {
    clColor :: Color,
    clPixels :: [Pixel]
} deriving (Show)

printClusters :: [Cluster] -> IO ()
printClusters = mapM_ printCluster

printCluster :: Cluster -> IO ()
printCluster cluster = do
    putStrLn "--"
    print (clColor cluster)
    putStrLn "-"
    mapM_ print (clPixels cluster)
