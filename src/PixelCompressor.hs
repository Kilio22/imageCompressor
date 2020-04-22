--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- PixelCompressor
--

module PixelCompressor
    ( compressPixels,
    getRandomIndex
    ) where

import DataTypes
import System.Random

deleteAt :: Int -> [Pixel] -> [Pixel]
deleteAt idx pixels = xs ++ ys
    where (xs, (_:ys)) = splitAt idx pixels

getRandomIndex :: [Pixel] -> IO (Int)
getRandomIndex pixels = newStdGen >>= (\randomSeed -> return (fst $ randomR (0, (length pixels) - 1) randomSeed))

compressPixels :: [Pixel] -> Int -> Float -> [Cluster]
compressPixels _ _ _ = []
