

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}

module IntegralBased  where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import           Linear

import           Control.Parallel.Strategies

import qualified Numeric.LinearAlgebra as LA
import           Numeric.GSL.Minimization
import           Numeric.FastMath()

{-import           GHC.Conc (numCapabilities)-}

import           PhiUpdates
import           Types
import           Common

-- | this function calculates a set of coefficients that
-- | scales the given phis to match the given patch best
-- | this is done by a gradient descent over the error
-- | function below
gradientDescentToFindAs :: Patch Double -> Phis Double -> As Double -> S.Vector Double
gradientDescentToFindAs patch phis randomAs = fst $ gradientDescentToFindAs' patch phis randomAs

gradientDescentToFindAs' :: Patch Double -> Phis Double -> As Double -> (S.Vector Double, LA.Matrix Double)
gradientDescentToFindAs' patch phis randomAs = 
    minimizeV NMSimplex2 10e-9 1000 (S.replicate (length phis) 1) errorFun (V.convert randomAs)
    where errorFun as = reconstructionError patch phis (V.convert as)
                      + sparseness as

sparseness :: (Floating a, S.Storable a) => S.Vector a -> a
sparseness as = S.sum $ S.map (\a -> log (1 + (a/σ)**2)) as
  where σ = 0.316 -- from olshausens code



--------------------------------------------------


oneIteration patches phis = (meanPhis,errorInits,errorAs,errorPhis)
    where 
          numPatches = V.length patches
          numPhis    = V.length phis

          -- fitting the as (one a per phi per patch)
          initialAs  = V.replicate (V.length patches) $ S.replicate (V.length phis) 1
          fittedAs   = withStrategy (parTraversable rdeepseq)
                     $ V.zipWith (\as patch -> gradientDescentToFindAs patch phis as) initialAs patches


          -- fit the phis
          fittedPhis :: V.Vector (Phis Double)
          fittedPhis = withStrategy (parTraversable rdeepseq)
                     $ V.zipWith (\as patch -> updatePhis patch phis as) fittedAs patches

          -- scale as so that the maximum is not more than one
          -- **8 is there to "convince" the phi to go in the direction of
          -- one patch
          {-maxA       = V.maximum . V.map S.maximum $ fittedAs-}
          {-scaledAs   = traceShowId $ V.map (S.map (\a -> (a / maxA)**16)) fittedAs-}
          maxAs     = V.foldl' (S.zipWith max) (S.replicate numPhis (-1/0)) fittedAs
          scaledAs  = V.map (\as -> S.zipWith (/) as maxAs) fittedAs

          -- scale phis according to scaled as and learning rate
          eta        = 0.1
          scaledPhis :: V.Vector (Phis Double)
          scaledPhis = V.zipWith (\as phis' -> go (V.convert as) phis') scaledAs fittedPhis
              where go as phis' 
                     = V.zipWith3 (\a phi phi' -> S.zipWith (\e e' -> e + ((eta * a) *^ (e' - e))) phi phi') as phis phis'

          meanPhis   = V.map (S.map (\e -> e ^/ fromIntegral numPatches))
                     . V.foldl1' (V.zipWith (S.zipWith (+))) 
                     $ scaledPhis

          errorInits = V.zipWith (\patch as -> reconstructionError patch phis as) patches initialAs
          errorAs    = V.zipWith (\patch as -> reconstructionError patch phis as) patches fittedAs
          errorPhis  = V.zipWith (\patch as -> reconstructionError patch meanPhis as) patches scaledAs




oneIterationPatch ::
  Patch Double
  -> Phis Double
  -> (Phis Double, Double, Double, Double)
oneIterationPatch patch phis = (scaledPhis, errorInit, errorAs, errorPhis)
    where initialAs = S.replicate (V.length phis) 1

          fittedAs = gradientDescentToFindAs patch phis initialAs
          fittedPhis = updatePhis patch phis fittedAs

          {-clampedAs  = traceShowId $ S.map (\a -> min 1 (max 0 a)) fittedAs-}
          clampedAs  = S.map (\a -> a / 5) fittedAs
          minA       = S.minimum fittedAs
          maxA       = S.maximum fittedAs
          scaledPhis = V.zipWith3 (\a -> S.zipWith (\e e' -> e + ((0.1 * a) *^ (e' - e)))) 
                                  (V.convert clampedAs) phis fittedPhis 

          errorInit = reconstructionError patch phis initialAs
          errorAs   = reconstructionError patch phis fittedAs
          errorPhis = reconstructionError patch scaledPhis clampedAs

          {-msg = printf "errors -> pre: %f, as: %f, phis: %f" errorInit errorAs errorPhis-}




{-testData = do-}
{-    patchA <- S.replicateM 32 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 0.0) :: IO (Patch Double)-}
{-    patchB <- S.replicateM 32 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 1.0) :: IO (Patch Double)-}
{-    let patches = V.fromList [patchA, patchB]-}

{-    phis  <- V.replicateM 2 $ S.replicateM 16 -}
{-                            $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> getRandomR (0,1)) :: IO (Phis Double)-}

{-    return (patches,phis)-}

{-test = do-}

{-    (patches,phis) <- testData-}


{-    let phis' = iterate (oneIteration (patches)) phis-}



{-    t <- formatTime defaultTimeLocale "%F_%T" <$> getCurrentTime-}
{-    let dn = "data/integration_based_" ++ t ++ "/" -}
{-    createDirectoryIfMissing True dn-}

{-    savePatches (dn ++ "patches.bin") patches-}


{-    forM_ (zip [0::Int ..] phis') $ \ (i,phi) -> do-}
{-      putStr $ "running iteration " ++ show i ++ " ... "-}
{-      tStart <- getCurrentTime-}
{-      encodeFile (dn ++ printf "phis_%05d.bin" i) (V.toList . V.map S.toList $ phi)-}
{-      tEnd <- getCurrentTime-}

{-      putStrLn $ "took " ++ show (tEnd .-. tStart)-}




{-    return (patches,phis,phis')-}




-------------- U T I L I T Y ------------------

mergeSpikes :: S.Storable a => V.Vector (S.Vector a) -> S.Vector a
mergeSpikes = V.foldl' (S.++) S.empty


{--- | add a coefficient to a spike-}
{-addA a (V3 x y z) = V4 a x y z-}
{-[># INLINABLE addA #<]-}
{--- | add the same coefficient to a range of spikes-}
{-addAs a vs = addA a <$> vs-}
{-[># INLINABLE addAs #<]-}
{--- | 'addAs' specialized for Storable Vectors-}
{-addAsS a vs = S.map (addA a) vs-}
{-[># INLINABLE addAsS #<]-}

-- | create 'V3' from 'Vector'
-- no checks are performed to guarantee that 'v' is of the correct size
{-unsafePackV3 v = V3 (v S.! 0) (v S.! 1) (v S.! 2)-}
{-[># INLINABLE unsafePackV3 #<]-}

{-unpackV3 v = S.fromListN 3 $ toList v-}
{-[># INLINABLE unpackV3 #<]-}


infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x

infixl 4 <$$$>
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
f <$$$> x = fmap (fmap (fmap f)) x
