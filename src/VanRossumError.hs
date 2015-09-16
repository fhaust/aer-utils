


module VanRossumError where

import           Numeric.LinearAlgebra
import           Numeric.GSL.Integration
import           Linear
import           Numeric.FastMath -- imports some optimization rules

import qualified Data.Vector.Storable as S

-- | calculate the van Rossum error between two spike trains
vanRossumError as bs = errorIntegral (as' S.++ bs')
  where as' = S.map (\(V3 x y z) -> (V4 1 x y z)) as
        bs' = S.map (\(V3 x y z) -> (V4 (-1) x y z)) bs

-- | this function calculates the integral of several
-- | gauss bells from -∞ to ∞
-- It takes it's arguments in the form of a vector with
-- several (V4 a x y z)
errorIntegral :: Vector (V4 Double) -> Double
errorIntegral vs = pi**(3/2) * fstSum + 2*pi**(3/2) * sndSum
  where fstSum = S.foldl' (\acc (V4 a _ _ _) -> acc + a**2) 0 vs
        sndSum = mapSumPairs go vs
        go (V4 a1 b1 c1 d1) (V4 a2 b2 c2 d2) = a1 * a2 * exp ( -0.25 * ( (b1-b2)**2 + (c1-c2)**2 + (d1-d2)**2  ) )


-- | combine every element of the vector with every other element,
-- | run a function on the pairings, then sum up the results
-- | this is super ungeneric :)
mapSumPairs ::
  (Num a, S.Storable b) => (b -> b -> a) -> S.Vector b -> a
mapSumPairs f = go
  where go vs = S.foldl' (\acc v -> acc + f (S.head vs) v) 0 (S.tail vs)
              + if S.length vs > 2 then go (S.tail vs) else 0
{-# INLINABLE mapSumPairs #-}



-- | numeric integration, mostly for comparison
gauss :: V4 Double -> Double
gauss (V4 a b c d) = a * exp( -0.5 * (b**2+c**2+d**2) )
errFun :: S.Vector (V4 Double) -> V3 Double -> Double
errFun gs (V3 x y z) = S.sum $ S.map (\(V4 a b c d) -> gauss (V4 a (x-b) (y-c) (z-d))) gs
squaredErrFun :: S.Vector (V4 Double) -> V3 Double -> Double
squaredErrFun gs v = (errFun gs v)**2

numericErrorIntegral :: S.Vector (V4 Double) -> (Double,Double)
numericErrorIntegral gs = integrateQAGI 1e-6 500 (\z -> fst $ integrateQAGI 1e-6 500 (\y -> fst $ integrateQAGI 1e-6 500 (\x -> squaredErrFun gs (V3 x y z)) ))
