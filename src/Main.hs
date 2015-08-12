
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import LearnGabors
import qualified Data.SimpleMat as M

import           Data.Fixed
import           Data.List.Split
import           Data.Thyme.Clock

import Codec.Picture
import Codec.Picture.Types


import           Control.Monad
import           Control.Exception
import           Control.DeepSeq
import           Control.Parallel.Strategies

import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U

import           Data.AER.TimeFront as DVS
import           Data.AER.DVS128 as DVS

import           System.Environment

import           GHC.TypeLits


import           OlshausenOnStreams

main = do
    {-(patches,fittedAs,reconstructedPhis,residualError) <- OlshausenOnStreams.test-}

    {-print fittedAs-}

    {-args@[ws,fn] <- getArgs-}

    {-unless (length args == 2) $ error "Usage: <prog> windowsize filename"-}

    {-(Right es) <- DVS.readDVSData fn-}

    {-let frontsST = createTimeFronts (read ws) es :: [M.Mat 128 128 Float]-}
    {-    fronts   = map (promoteImage . timeFrontToImage) frontsST :: [Image PixelRGB16]-}

    {-zipWithM_ (\i f -> writePng ("output/fronts/front-" ++ show i ++ ".png") f) [0..] fronts-}

    putStrLn "done"

convertImage from to = do
    f <- readFile from
    let ss = map (U.fromList . map read . splitOn ",") $ lines f :: [U.Vector Double]
        i  = generateImage (\x y -> realToFrac ((ss !! y) U.! x)) 512 512 :: Image PixelF
    writeHDR to (promoteImage i)










normalizeMat m = fmap (\a -> (a - low) / (high - low)) m
    where low = minimum m
          high = maximum m


createTimeFronts :: NominalDiffTime -> [DVS.Event DVS.Address] -> [M.Mat 128 128 Float]
createTimeFronts windowSize es = ts

    where ts = map ( normalizeMat  -- normalize to window sizes
                   . M.mkMatU . U.toList . U.map (realToFrac . (`mod'` windowSize) . fst) -- convert to matrix
                   . timeFront           -- create timefronts
                   )
             . filter ((>50) . length) -- only use windows with at least some content
             . extractWindows windowSize 
             $ es 



timeFrontToImage :: (KnownNat w, KnownNat h, RealFrac a) => M.Mat w h a -> Image Pixel16
timeFrontToImage ts = generateImage go (M.width ts) (M.height ts)
    where go x y = round . (*2^16) $ M.index ts x y
