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

distance :: Color -> Color -> Float
distance (Color a b c) (Color d e f) = sqrt ((fromIntegral a - fromIntegral d)**2 + (fromIntegral b - fromIntegral e)**2 + (fromIntegral c - fromIntegral f)**2)

getAverage :: [Pixel] -> Color -> Float
getAverage list ref = (sum $ fmap (\pixel -> distance ref (piColor pixel)) list) / (fromIntegral (length list))

isClosest :: Float -> [Cluster] -> Pixel -> Bool
isClosest _ [] _ = True
isClosest clusterDistance (x:xs) pixel
    | distance (clColor x) (piColor pixel) < clusterDistance = False
    | otherwise = isClosest clusterDistance xs pixel

getNewCluster :: Cluster -> [Cluster] -> [Pixel] -> Cluster
getNewCluster cluster _ [] = cluster
getNewCluster cluster clusters (y:ys) = case isClosest (distance (clColor cluster) (piColor y)) clusters y of
                                    True -> getNewCluster (Cluster (clColor cluster) (y : (clPixels cluster))) clusters ys
                                    False -> getNewCluster cluster clusters ys

comparePixels :: Pixel -> Pixel -> Bool
comparePixels (Pixel (Point x0 y0) (Color r0 g0 b0)) (Pixel (Point x1 y1) (Color r1 g1 b1)) = x0 == x1 && y0 == y1 && r0 == r1 && g0 == g1 && b0 == b1

getNewClusters :: [Cluster] -> [Pixel] -> [Cluster]
getNewClusters clusters [] = clusters
getNewClusters (x:xs) pixels = do
    let newCluster = getNewCluster (Cluster (clColor x) []) xs pixels
    let newPixels = filter (\pixel -> (length (filter (\clusterPixel -> comparePixels pixel clusterPixel) (clPixels newCluster))) == 0) pixels
    case length (clPixels newCluster) > 0 of
            True -> Cluster (getPixelsColorAverage (clPixels newCluster)) (clPixels newCluster) : getNewClusters xs newPixels
            False -> getNewClusters xs newPixels

checkLimit :: [Cluster] -> [Cluster] -> Float -> Bool
checkLimit [] _ _ = True
checkLimit (x:xs) (y:ys) limit
    | (abs averageDifference) < limit = checkLimit xs ys limit
    | otherwise = False
    where averageDifference = (getAverage (clPixels x) (clColor x)) - (getAverage (clPixels y) (clColor y))

updateUntilConvergence :: [Cluster] -> [Pixel]-> Float -> IO ([Cluster])
updateUntilConvergence clusters pixels limit = do
    let newClusters = getNewClusters clusters pixels
    case checkLimit newClusters clusters limit of
        True -> return (newClusters)
        False -> updateUntilConvergence newClusters pixels limit

compressPixels :: [Pixel] -> Int -> Float -> IO ([Cluster])
compressPixels pixels n limit = do
    shuffledPixels <- getPixels pixels
    let shuffledClusters = createShuffledClusters (splitList shuffledPixels ((length shuffledPixels) `div` n) n) n
    updateUntilConvergence shuffledClusters pixels limit