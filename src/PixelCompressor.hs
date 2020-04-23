--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- PixelCompressor
--

module PixelCompressor
    ( compressPixels,
    getRandomIndex,
    splitList
    ) where

import DataTypes (Cluster(..), Pixel(..), Color(..))
import System.Random
import Data.List

deleteAt :: Int -> [Pixel] -> [Pixel]
deleteAt idx pixels = xs ++ ys
    where (xs, (_:ys)) = splitAt idx pixels

getRandomIndex :: [Pixel] -> IO (Int)
getRandomIndex pixels = newStdGen >>= (\randomSeed -> return (fst $ randomR (0, (length pixels) - 1) randomSeed))

splitList :: [Pixel] -> Int -> Int -> [[Pixel]]
splitList pixels _ 1 = [pixels]
splitList [] _ _ = []
splitList pixels nbPixelByList it = fst splitedList : splitList (snd splitedList) nbPixelByList (it - 1)
    where splitedList = splitAt nbPixelByList pixels

getPixels :: [Pixel] -> IO ([Pixel])
getPixels [] = return ([])
getPixels pixelList = do
    randNb <- getRandomIndex pixelList
    pixels <- getPixels (deleteAt randNb pixelList)
    return ([pixelList!!randNb] ++ pixels)

getClusters :: [[Pixel]] -> Int -> IO ([Cluster])
getClusters _ 0 = return ([])
getClusters (x:xs) n = do
    pixels <- getPixels x
    clusters <- getClusters xs (n - 1)
    return (Cluster (Color 0 0 0) pixels : clusters)

compressPixels :: [Pixel] -> Int -> Float -> IO ([Cluster])
compressPixels pixels n _ = getClusters (splitList pixels ((length pixels) `div` n) n) n
