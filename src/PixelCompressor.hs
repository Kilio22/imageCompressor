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

import DataTypes (Pixel(..), Color(..), Point(..))
import Clusters (Cluster(..), printClusters)
import System.Random
import Data.List

deleteAt :: Int -> [Pixel] -> [Pixel]
deleteAt idx pixels = xs ++ ys
    where (xs, (_:ys)) = splitAt idx pixels

getRandomIndex :: [Pixel] -> Int -> IO (Int)
getRandomIndex pixels listSize = do
    randomSeed <- newStdGen
    case listSize > 200 of
        True -> return (fst $ randomR (0, 200) randomSeed)
        False -> return (fst $ randomR (0, listSize - 1) randomSeed)

splitList :: [Pixel] -> Int -> Int -> [[Pixel]]
splitList pixels _ 1 = [pixels]
splitList [] _ _ = []
splitList pixels nbPixelByList it = fst splitedList : splitList (snd splitedList) nbPixelByList (it - 1)
    where splitedList = splitAt nbPixelByList pixels

getShuffledPixels :: [Pixel] -> Int -> IO ([Pixel])
getShuffledPixels [] 0 = return ([])
getShuffledPixels pixelList listSize = do
    randNb <- getRandomIndex pixelList listSize
    pixels <- getShuffledPixels (deleteAt randNb pixelList) (listSize - 1)
    return (pixelList!!randNb: pixels)

getColorAverage :: [Pixel] -> (Color -> Int) -> Int
getColorAverage [] _ = 0
getColorAverage (x:xs) fct = (fct $ piColor x) + getColorAverage xs fct

getPixelsColorAverage :: [Pixel] -> Color
getPixelsColorAverage pixels = do
    let redAverage = ceiling $ fromIntegral (getColorAverage pixels r) / fromIntegral (length pixels)
    let greenAverage = ceiling $ fromIntegral (getColorAverage pixels g) / fromIntegral (length pixels)
    let blueAverage = ceiling $ fromIntegral (getColorAverage pixels b) / fromIntegral (length pixels)
    Color redAverage greenAverage blueAverage

createShuffledClusters :: [[Pixel]] -> Int -> [Cluster]
createShuffledClusters _ 0 = []
createShuffledClusters (x:xs) n = Cluster (getPixelsColorAverage x) x : createShuffledClusters xs (n - 1)

distance :: Color -> Color -> Int
distance (Color a b c) (Color d e f) = (a - d)^2 + (b - e)^2 + (c - f)^2

getAverage :: [Pixel] -> Color -> Int
getAverage list ref = (sum $ fmap (\pixel -> distance ref (piColor pixel)) list) `div` (length list)

isClosest :: Int -> [Cluster] -> Pixel -> Bool
isClosest _ [] _ = True
isClosest clusterDistance (x:xs) pixel
    | distance (clColor x) (piColor pixel) < clusterDistance = False
    | otherwise = isClosest clusterDistance xs pixel

updateClusters :: [Cluster] -> [Cluster]
updateClusters [] = []
updateClusters (x:xs) = case (clPixels x) == [] of
                                True -> updateClusters xs
                                False -> (Cluster (getPixelsColorAverage (clPixels x)) (clPixels x)) : updateClusters xs

putPixel :: [Cluster] -> Pixel -> [Cluster]
putPixel [] _ = []
putPixel (x:xs) pixel = case isClosest (distance (clColor x) (piColor pixel)) xs pixel of
                                    False -> x : putPixel xs pixel
                                    True -> (Cluster (clColor x) (pixel : (clPixels x))) : xs

getNewClusters :: [Cluster] -> [Pixel] -> [Cluster]
getNewClusters clusters [] = clusters
getNewClusters clusters (x:xs) = getNewClusters (putPixel clusters x) xs

resetClusters :: [Cluster] -> [Cluster]
resetClusters [] = []
resetClusters (x:xs) = (Cluster (clColor x) []) : resetClusters xs

checkLimit :: [Cluster] -> [Cluster] -> Float -> Bool
checkLimit [] _ _ = True
checkLimit (x:xs) (y:ys) limit
    | (fromIntegral (abs averageDifference)) < limit = checkLimit xs ys limit
    | otherwise = False
    where averageDifference = (getAverage (clPixels x) (clColor x)) - (getAverage (clPixels y) (clColor y))

updateUntilConvergence :: [Cluster] -> [Pixel]-> Float -> IO ([Cluster])
updateUntilConvergence clusters pixels limit = do
    let newClusters = updateClusters $ getNewClusters (resetClusters clusters) pixels
    case checkLimit newClusters clusters limit of
        True -> return (newClusters)
        False -> updateUntilConvergence newClusters pixels limit

compressPixels :: [Pixel] -> Int -> Float -> IO ([Cluster])
compressPixels pixels n limit = do
    shuffledPixels <- getShuffledPixels pixels (length pixels)
    let shuffledClusters = createShuffledClusters (splitList shuffledPixels ((length shuffledPixels) `div` n) n) n
    updateUntilConvergence shuffledClusters pixels (limit^2)