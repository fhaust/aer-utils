

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module IntegralBased (test, eventDistance, gradientDescentToFindAs) where

import           System.IO.Unsafe
 
import           Data.Foldable
import           Data.Bifunctor
import           Data.List

import qualified Data.Vector as V
{-import qualified Data.Vector.Unboxed as U-}
import           Linear

import           Math.GaussianQuadratureIntegration

import qualified Numeric.AD as AD
import qualified Numeric.AD.Mode as AD
import qualified Numeric.AD.Internal.Reverse as AD

import           Control.Monad
import           Control.Monad.Random
import           Control.Lens ((^.))

import           Debug.Trace

import           Monte
import           Orphans


type Event a  = V3 a
type Events a = V.Vector (Event a)

type Patch a = Events a
type Phi a   = Events a
type As a    = V.Vector a

type Phis a  = V.Vector (Phi a)

gradientDescentToFindAs :: forall a. (Show a, Ord a, Floating a, Random a, Show a) => Patch a -> Phis a -> As a -> [As a]
gradientDescentToFindAs patch phis randomAs = AD.gradientDescent go randomAs
  where go :: forall t. (AD.Scalar t ~ a, AD.Mode t, Floating t, Ord t, Random t, Show t) => V.Vector t -> t
        go as = eventDistance (AD.auto <$$> patch) (AD.auto <$$$> phis) as

eventDistance :: (Ord a, Floating a, Random a, Show a) => Patch a -> Phis a -> As a -> a
eventDistance patch phis coeffs = Debug.Trace.trace "eventDistance" $ nIntegrate3d' totalFun (V3 0 0 0) (V3 128 128 1) 
    where totalFun v = (phisFun v + patchFun v) ^ (2::Int)
          phisFun v = V.sum $ V.zipWith (\c -> V.sum . V.map (\a -> gauss c a identity v)) coeffs phis
          patchFun v = V.sum $ V.map (\b -> gauss (-1) b identity v) patch

nIntegrate3d :: Fractional a => (V3 a -> a) -> V3 a -> V3 a -> a
nIntegrate3d f (V3 lx ly lz) (V3 hx hy hz) = nIntegrate128 (\z -> nIntegrate128 (\y -> nIntegrate128 (\x -> f (V3 x y z)) lx hx) ly hy) lz hz

nIntegrate3d' :: (Floating a, Ord a, Random a, Show a) => (V3 a -> a) -> V3 a -> V3 a -> a
nIntegrate3d' f (V3 lx ly lz) (V3 hx hy hz) = unsafePerformIO go
    where go = fst <$> recursiveStratifiedSampling 1 1000 f (V4 lx ly lz (-1)) (V4 hx hy hz 1)



{-gauss :: Floating a => a -> V3 a -> M33 a -> V3 a -> a-}
gauss a b c x = a * exp ( Linear.dot (negate (x-b) *! c) (x-b))
{-# INLINABLE gauss #-}



test = do


    patch <- V.replicateM 16 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 0) :: IO (Patch Float)
    phis  <- V.replicateM 2 $ V.replicateM 16 
                            $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> getRandomR (0,1)) :: IO (Phis Float)

    randomAs <- V.replicateM 2 getRandom :: IO (As Float)


    let fittedAs = gradientDescentToFindAs patch phis randomAs

    putStrLn $ show (fittedAs !! 0)

    return (patch, phis, randomAs)



infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x

infixl 4 <$$$>
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
f <$$$> x = fmap (fmap (fmap f)) x



instance (Show a, Num a, Random a) => Random (AD.Reverse s a) where
    randomR (AD.Lift l, AD.Lift h) = runRand (AD.Lift <$> getRandomR (l,h))
    randomR (AD.Zero, AD.Zero)     = runRand (return AD.Zero)
    randomR (AD.Zero, AD.Lift h)   = runRand (AD.Lift <$> getRandomR (0,h))
    {-randomR (l,h)                  = runRand (traceM ("l: " ++ show l ++ " h: " ++ show h) >> return AD.Zero)-}
    random = runRand (AD.Lift <$> getRandom)
