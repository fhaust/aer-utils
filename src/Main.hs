
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import LearnGabors
import Data.SimpleMat

import           Data.List.Split

import Codec.Picture
import Codec.Picture.Types

import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U

main = learnGabors

convertImage from to = do
    f <- readFile from
    let ss = map (U.fromList . map read . splitOn ",") $ lines f :: [U.Vector Double]
        i  = generateImage (\x y -> realToFrac ((ss !! y) U.! x)) 512 512 :: Image PixelF
    writeHDR to (promoteImage i)
