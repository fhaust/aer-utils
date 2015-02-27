
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Data.AER.SIFT where



import           Data.AER.DVS128
import           Data.AER.Types

import           System.Environment

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

import           Data.Thyme.Clock
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Traversable
import           Data.Foldable

import           Control.Applicative

import           Numeric.LinearAlgebra.HMatrix

import           Prelude hiding (mapM)

-- kernels as calculated by:
-- http://dev.theomader.com/gaussian-kernel-calculator/

kernel1x3 = matrix 3 [0.077847,0.123317,0.077847
                     ,0.123317,0.195346,0.123317
                     ,0.077847,0.123317,0.077847
                     ]

kernels = take 8 $ iterate (conv2 kernel1x3) (matrix 1 [1])

imgSizes = take 8 $ iterate (`div` 2) 128

kernelSizes = take 8 $ map (fst . size) kernels

globalLeak = 0.1

type Neuron = (Float,UTCTime)

updateNeuron :: Float -> Float -> Neuron -> UTCTime -> (Bool,Neuron)
updateNeuron l w (v,t) now = do
    let dt = toSeconds (now .-. t)
        v' = max 0 (v - dt * l) + w

    if (v' > 1)
      then (True,(0,now))
      else (False,(v',now))



{-updatePyramidLevel :: Int -> UMV.IOVector Neuron -> UTCTime -> Int -> Int -> Float -> IO ()-}
updatePyramidLevel level accs now p x y cont = do
    -- calculate level coords
    let x' = x `div` 2
        y' = y `div` 2
        ls = 128 `div` 2^level
        li = x' + y' * ls

    oldNeuron <- UMV.read accs li
    let (spike,newNeuron) = updateNeuron globalLeak (p*0.25) oldNeuron now
    UMV.write accs li newNeuron

    let spikeDesc = (level,x',y',p)
    if spike 
      then (spikeDesc:) <$> cont x' y'
      else return []

updatePyramid :: [UMV.IOVector Neuron] -> UTCTime -> Float -> Int -> Int
              -> IO [(Int, Int, Int, Float)]
updatePyramid accs now p x y = do

    updatePyramidLevel 1 (accs !! 1) now p x y $ \x y -> do
      putStrLn $ "level " ++ show 1 ++ " fired at " ++ show x ++ " " ++ show y
      updatePyramidLevel 2 (accs !! 2) now p x y $ \x y -> do
        putStrLn $ "level " ++ show 2 ++ " fired at " ++ show x ++ " " ++ show y
        updatePyramidLevel 3 (accs !! 3) now p x y $ \x y -> do
          putStrLn $ "level " ++ show 3 ++ " fired at " ++ show x ++ " " ++ show y
          updatePyramidLevel 4 (accs !! 4) now p x y $ \x y -> do
            putStrLn $ "level " ++ show 4 ++ " fired at " ++ show x ++ " " ++ show y
            updatePyramidLevel 5 (accs !! 5) now p x y $ \x y -> do
              putStrLn $ "level " ++ show 5 ++ " fired at " ++ show x ++ " " ++ show y
              updatePyramidLevel 6 (accs !! 6) now p x y $ \x y -> do
                putStrLn $ "level " ++ show 6 ++ " fired at " ++ show x ++ " " ++ show y
                updatePyramidLevel 7 (accs !! 7) now p x y $ \x y -> do
                  putStrLn $ "level " ++ show 7 ++ " fired at " ++ show x ++ " " ++ show y
                  return []

unP U = 1
unP D = -1

isUEvent (qview -> (p,_,_,_)) = p == U
isDEvent (qview -> (p,_,_,_)) = p == D

infix 4 <$$>
f <$$> x = fmap (fmap f) x



main = do

    startTime <- getCurrentTime
    accs <- mapM (\s -> UMV.replicate (s*s) (0,startTime)) imgSizes :: IO [UMV.IOVector Neuron]


    -- read aedat file as specified on the commandline
    [filename] <- getArgs

    (Right es) <- V.filter isUEvent . V.fromList <$$> readDVSData filename
    
    spikes <- forM es $ \(qview -> (p,x,y,t)) -> do
      let now = startTime .+^ t
      updatePyramid accs now (unP p) (fromIntegral x) (fromIntegral y)
    
    let spikes' = asum spikes

    putStrLn "done"
