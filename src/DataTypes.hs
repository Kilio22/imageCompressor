--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- DataTypes
--

module DataTypes
    ( Point
    , Color
    , Pixel
    , Cluster
    ) where

import Data.Word (Word8)

data Point = Point {
    x :: Int,
    y :: Int
}

data Color = Color {
    r :: Word8,
    g :: Word8,
    b :: Word8
}

data Pixel = Pixel {
    piPoint :: Point,
    piColor :: Color
}

data Cluster = Cluster {
    clColor :: Color,
    clPixels :: [Pixel]
}
