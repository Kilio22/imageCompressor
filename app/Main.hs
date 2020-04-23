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
                                                , Cluster
                                                )
import           PixelCompressor                ( compressPixels )
import           Data.Word                      ( Word8 )

printClusters :: [Cluster] -> IO ()
printClusters = error "Putain mais tamer"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [nColorsStr, nLimitStr, file] -> do
            let nColors = fromMaybe (error "Bad 'n' parameter") (readMaybe nColorsStr :: Maybe Int)
            let nLimit = fromMaybe (error "Bad 'e' parameter") (readMaybe nLimitStr :: Maybe Float)

            mbPixels <- getPixelsFromFile file
            let pixels = fromMaybe (error "Bad 'file' paramter") mbPixels

            print nColors
            print nLimit
            print pixels

            clusters <- compressPixels pixels nColors nLimit

            print clusters
        _ -> error "Wrong arguments"

getPixelsFromFile :: FilePath -> IO (Maybe [Pixel])
getPixelsFromFile file = do
    fileContents <- readFile file
    let fileLines    = lines fileContents
    let fileMbPixels = fmap readPixel fileLines
    return (sequence fileMbPixels)

-- (0, 0) (255, 255, 255)
readPixel :: String -> Maybe Pixel
readPixel line = do
    let [pointStr, colorStr] = words line
    -- xd <- case length pixelStr of
    --     2 -> pixelStr
    --     _ -> Nothing
    -- let point = Point 0 0
    -- let color = Color 0 0 0
    let (px, py) = fromMaybe (error "Bad file format") (readMaybe pointStr :: Maybe (Int, Int))
    let (cr, cg, cb) = fromMaybe (error "Bad file format") (readMaybe colorStr :: Maybe (Word8, Word8, Word8))
    let point = Point px py
    let color = Color cr cg cb
    Just (Pixel point color)
