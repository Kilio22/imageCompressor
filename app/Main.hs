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
readPixel line = case words line of
    [pointStr, colorStr] -> do
        let (px, py) = fromMaybe (error "Bad point format") (readMaybe pointStr :: Maybe (Int, Int))
        let point = Point px py

        let ct = fromMaybe (error "Bad color format") (readMaybe colorStr :: Maybe (Int, Int, Int))
        let color = fromMaybe (error "Bad color value range") (makeColor ct)

        Just (Pixel point color)
    _ -> error "Bad line format in file"

makeColor :: (Int, Int, Int) -> Maybe Color
makeColor (cr, cg, cb)
    | cr >= 0 && cr <= 255 && cg >= 0 && cg <= 255 && cb >= 0 && cb <= 255
    = Just (Color cr cg cb)
    | otherwise
    = Nothing
