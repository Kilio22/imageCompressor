--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- Main
--

module Main where

import System.Environment (getArgs)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, fromMaybe)
import Text.Read (readMaybe)
import Data.Ord (Down(..))
import DataTypes (Point, Color, Pixel, Cluster)
import PixelCompressor (compressPixels)

printClusters :: [Cluster] -> IO ()
printClusters = error "Putain mais tamer"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file, pixelStr] -> do
            fileContents <- readFile file
            let fileLines = lines fileContents -- :: [String]
            let fileMbPixels = fmap readMaybe fileLines -- :: [Maybe Pixel]
            let mbFilePixels = sequence fileMbPixels -- :: Maybe [Pixel]

            let filePixels = fromMaybe (error "Bad file") mbFilePixels
            let pixel = fromMaybe (error "Wrong point argument") (readMaybe pixelStr)

            print $ closest filePixels pixel
        _ -> error "Wrong arguments"

type Pixelle = (Int, Int, Int)

-- liste :: [Pixel]
-- liste = [(33,18,109), (33,17,109), (35,18,111), (0, 0, 0), (10, 10, 10), (35,21,109), (38,21,112)]

distance :: Pixelle -> Pixelle -> Float
distance (x0, y0, z0) (x1, y1, z1) = sqrt (fromIntegral $ (x0 - x1)^2 + (y0 - y1)^2 + (z0 - z1)^2)

-- readInputFile :: IO String
-- readInputFile = head <$> getArgs >>= readFile

closest :: [Pixelle] -> Pixelle -> Maybe Pixelle
closest l point = listToMaybe $ sortOn (distance point) l

-- expelOutliers :: [Pixel] -> [Pixel]
-- expelOutliers pixels = drop 1 (sortOn (Down . totalDistance) pixels)
--     where totalDistance pixel = sum (distance pixel <$> pixels)
