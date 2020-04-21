module Main where

import System.Environment (getArgs)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, fromMaybe)
import Text.Read (readMaybe)
import Data.Ord (Down(..))
import Lib

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

type Pixel = (Int, Int, Int)

distance :: Pixel -> Pixel -> Float
distance (x0, y0, z0) (x1, y1, z1) = sqrt (fromIntegral $ (x0 - x1)^2 + (y0 - y1)^2 + (z0 - z1)^2)

closest :: [Pixel] -> Pixel -> Maybe Pixel
closest l point = listToMaybe $ sortOn (distance point) l

