--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- DataTypes
--

module DataTypes
    ( Point(..)
    , Color(..)
    , Pixel(..)
    )
where

data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show)

data Color = Color {
    r :: Int,
    g :: Int,
    b :: Int
} deriving (Show)

data Pixel = Pixel {
    piPoint :: Point,
    piColor :: Color
} deriving (Show)
