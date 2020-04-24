--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- Main
--

module Main where

import           System.Exit
import           System.Environment             ( getArgs )
import           System.IO
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
import           Control.Exception

main :: IO ()
main = do
    args <- getArgs
    case args of
        [nColorsStr, nLimitStr, file] -> do
            nColors <- case readMaybe nColorsStr :: Maybe Int of
                Nothing -> hPutStrLn stderr "Bad 'n' parameter" >> exitWith (ExitFailure 84)
                Just n -> return n
            nLimit <- case readMaybe nLimitStr :: Maybe Float of
                Nothing -> hPutStrLn stderr "Bad 'e' parameter" >> exitWith (ExitFailure 84)
                Just n -> return n

            mbPixels <- getPixelsFromFile file
            pixels   <- case mbPixels of
                Nothing -> hPutStrLn stderr "Bad 'file' paramter" >> exitWith (ExitFailure 84)
                Just p -> return p

            clusters <- compressPixels pixels nColors nLimit

            printClusters clusters
        _ -> hPutStrLn stderr "Wrong parameter count (2 required)"
            >> exitWith (ExitFailure 84)

getPixelsFromFile :: FilePath -> IO (Maybe [Pixel])
getPixelsFromFile file = do
    fileContents <- catch
        (readFile file)
        (\e -> do
            let err = show (e :: IOException)
            hPutStrLn stderr "Couldn't open file"
            return ""
        )

    let fileLines    = lines fileContents
    let fileMbPixels = fmap readPixel fileLines
    case length fileMbPixels of
        0 -> return Nothing
        _ -> return (sequence fileMbPixels)

readPixel :: String -> Maybe Pixel
readPixel line = case words line of
    [pointStr, colorStr] -> do
        (px, py) <- readMaybe pointStr :: Maybe (Int, Int)
        let point = Point px py

        ct    <- readMaybe colorStr :: Maybe (Int, Int, Int)
        color <- makeColor ct

        Just (Pixel point color)
    _ -> Nothing

makeColor :: (Int, Int, Int) -> Maybe Color
makeColor (cr, cg, cb)
    | cr >= 0 && cr <= 255 && cg >= 0 && cg <= 255 && cb >= 0 && cb <= 255
    = Just (Color cr cg cb)
    | otherwise
    = Nothing
