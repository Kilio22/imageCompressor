--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- DataTypes
--

module DataTypes
    ( Point(..)
    , printPoint
    , Color(..)
    , printColor
    , Pixel(..)
    , printPixel
    )
where

import Text.Printf (printf)

data Point = Point {
    x :: Int,
    y :: Int
} deriving Eq

printPoint :: Point -> IO ()
printPoint point = printf "(%d,%d)" (x point) (y point)

data Color = Color {
    r :: Int,
    g :: Int,
    b :: Int
} deriving Eq

printColor :: Color -> IO ()
printColor color = printf "(%d,%d,%d)" (r color) (g color) (b color)

data Pixel = Pixel {
    piPoint :: Point,
    piColor :: Color
} deriving Eq

printPixel :: Pixel -> IO ()
printPixel pixel = printPoint (piPoint pixel) >> putStr " " >> printColor (piColor pixel)
