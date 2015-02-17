
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import qualified Data.AER.DVS128 as DVS
import qualified Data.Vector     as V

import           Control.Applicative
import           Control.Monad.Random
import           Control.Monad
import           Control.Parallel.Strategies

import           Data.List
import           Data.Word7
import           Data.Complex
import           Data.Gabor
import           Data.Monoid
import           Data.MemoTrie
import qualified Data.Foldable as F

import           GA

import           System.Random

import           Codec.Picture

newtype Gabor  = Gabor {unGabor :: (Double,Double,Double,Double,Double,Double,Double)} deriving (Show,Read,Ord,Eq)

mkGabor λ θ ψ σ γ ox oy = Gabor (λ,θ,ψ,σ,γ,ox,oy)
evalGabor (Gabor (λ,θ,ψ,σ,γ,ox,oy)) x y = gabor λ θ ψ σ γ (x-ox) (y-oy)

type    Score  = Double
type    Events = V.Vector (DVS.Event DVS.Address)
type    Pool   = ()

{-linlin minA maxA minB maxB x = (((x - minA) / (maxA - minA)) * (maxB - minB)) + minB          -}

{-hot f = PixelRGBF r g b-}
{-    where r = min 255 . max 0 $ linlin 0 1 0    2.5 f-}
{-          g = min 255 . max 0 $ linlin 0 1 (-1) 1.5 f-}
{-          b = min 255 . max 0 $ linlin 0 1 (-4) 1   f-}

{-gaborImage λ θ ψ σ γ = pixelMap hot (generateImage (\ (fromIntegral -> x) (fromIntegral -> y) -> (/2) . (+1) . realPart $ gabor λ θ ψ σ γ ((x-128)/64) ((y-128)/64)) 256 256 :: Image PixelF)-}
{-gaborImage' g  = pixelMap hot (generateImage (\ (fromIntegral -> x) (fromIntegral -> y) -> realToFrac . (/2) . (+1) . realPart $ evalGabor g x y) 256 256 :: Image PixelF)-}



-- | generate a single random gabor filter
genRandomGabor :: (Applicative m, MonadRandom m) => m Gabor
genRandomGabor = mkGabor <$> getRandomR (0,128)    -- wavelength
                         <*> getRandomR (-pi,pi)   -- orientation
                         <*> getRandomR (0,1)      -- phase
                         <*> getRandomR (0,64)     -- size
                         <*> getRandomR (0,10)     -- compression
                         <*> getRandomR (0,128)    -- offset x
                         <*> getRandomR (0,128)    -- offset y


-- | combine two gabor filters into one
crossoverGabors :: (Applicative m, MonadRandom m) => Gabor -> Gabor -> m Gabor
crossoverGabors (Gabor (aλ,aθ,aψ,aσ,aγ,aox,aoy)) (Gabor (bλ,bθ,bψ,bσ,bγ,box,boy)) = go
    where go = mkGabor <$> uniform [aλ,bλ]
                       <*> uniform [aθ,bθ]
                       <*> uniform [aψ,bψ]
                       <*> uniform [aσ,bσ]
                       <*> uniform [aγ,bγ]
                       <*> uniform [aox,box]
                       <*> uniform [aoy,boy]

-- | mutate one gabor filter
mutateGabor :: (Applicative m, MonadRandom m) => Gabor -> m Gabor
mutateGabor (Gabor (λ,θ,ψ,σ,γ,ox,oy) ) = mkGabor <$> uniform [0.9*λ,λ,1.1*λ]
                                                 <*> uniform [0.9*θ,θ,1.1*θ]
                                                 <*> uniform [0.9*ψ,ψ,1.1*ψ] 
                                                 <*> uniform [0.9*σ,σ,1.1*σ]
                                                 <*> uniform [0.9*γ,γ,1.1*γ]
                                                 <*> uniform [0.9*ox,ox,1.1*ox]
                                                 <*> uniform [0.9*oy,oy,1.1*oy]

newtype ListSum a = ListSum { unListSum :: [a] } deriving (Show,Read,Eq,Ord)

instance (Num a) => Monoid (ListSum a) where
    mempty      = ListSum $ repeat 0
    mappend a b = ListSum $ zipWith (+) (unListSum a) (unListSum b)


scoreGabor :: Events -> Gabor -> Double
scoreGabor es g = negate $ V.foldl' go 0 es
    where go acc (DVS.qview -> (p,x,y,t)) = acc + unP p * preGabor (fromIntegral x) (fromIntegral y)
          unP DVS.U = 1
          unP DVS.D = -1
          preGabor x y = v V.! (x + y * 128)
            where v = V.generate (128*128) (\i -> let (y,x) = i `quotRem` 128 in realPart $ evalGabor g (fromIntegral x) (fromIntegral y))

scoreGaborPop :: Events -> [Gabor] -> [Double]
scoreGaborPop es gs = parMap rseq (scoreGabor es) gs


-- define an instance of Entitiy for Gabor filters over AER streams
instance Entity Gabor Score Events Pool IO where
    genRandom pool seed           = return $ evalRand genRandomGabor (mkStdGen seed)
    crossover pool param seed a b = return $ evalRand (Just <$> crossoverGabors a b) (mkStdGen seed)
    mutation  pool param seed g   = return $ evalRand (Just <$> mutateGabor g) (mkStdGen seed) 
    scorePop  es universe pop     = return . Just . map Just $ scoreGaborPop es pop
    {-score' events gabor           = Just $ scoreGabor events gabor-}




main :: IO ()
main = do

  -- get a random number generator
  let gen = mkStdGen 1337

  -- ga configuration
  let gaConfig = GAConfig
        { getPopSize        = 10000
        , getArchiveSize    = 10
        , getMaxGenerations = 10
        , getCrossoverRate  = 0.8
        , getMutationRate   = 0.2
        , getCrossoverParam = 0
        , getMutationParam  = 0
        , getWithCheckpointing = False
        , getRescoreArchive = False
        }

  -- read in chessboard dataset
  (Right chessboard) <- fmap V.fromList <$> DVS.readDVSData "/home/florian/Dokumente/Studium/NE/master/haskell/aer/data/chessboard.aedat"

  -- run the ga
  result <- evolveVerbose (mkStdGen 1337) gaConfig () chessboard :: IO (Archive Gabor Score)


  print result

  
