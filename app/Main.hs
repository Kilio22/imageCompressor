--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- Main
--

module Main where

import           System.Environment             ( getArgs )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( listToMaybe
                                                , fromMaybe
                                                )
import           Text.Read                      ( readMaybe )
import           Data.Ord                       ( Down(..) )
import           DataTypes                      ( Point(..)
                                                , Color(..)
                                                , Pixel(..)
                                                )
import           Clusters                       ( Cluster
                                                , printClusters
                                                )

import           PixelCompressor                ( compressPixels )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [nColorsStr, nLimitStr, file] -> do
            let nColors = fromMaybe (error "Bad 'n' parameter") (readMaybe nColorsStr :: Maybe Int)
            let nLimit = fromMaybe (error "Bad 'e' parameter") (readMaybe nLimitStr :: Maybe Float)

            mbPixels <- getPixelsFromFile file
            let pixels = fromMaybe (error "Bad 'file' paramter") mbPixels

            clusters <- compressPixels pixels nColors nLimit

            printClusters clusters
        _ -> error "Wrong arguments"

getPixelsFromFile :: FilePath -> IO (Maybe [Pixel])
getPixelsFromFile file = do
    fileContents <- readFile file
    let fileLines    = lines fileContents
    let fileMbPixels = fmap readPixel fileLines
    return (sequence fileMbPixels)

readPixel :: String -> Maybe Pixel
readPixel line = do
    let [pointStr, colorStr] = words line
    -- xd <- case length pixelStr of
    --     2 -> pixelStr
    --     _ -> Nothing
    -- let point = Point 0 0
    -- let color = Color 0 0 0
    let (px, py) = fromMaybe (error "Bad file format") (readMaybe pointStr :: Maybe (Int, Int))
    let (cr, cg, cb) = fromMaybe (error "Bad file format") (readMaybe colorStr :: Maybe (Int, Int, Int))
    let point = Point px py
    let color = Color cr cg cb
    Just (Pixel point color)
