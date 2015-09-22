

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}

module IntegralBased  where

import           Data.Number.Erf
import           Data.List
import           Data.Binary
import           Data.Thyme
import           Data.Foldable
import           Data.AffineSpace

import           Text.Printf

import           System.Directory
import           System.Locale

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Generic as G
{-import qualified Data.Vector.Unboxed as U-}
import           Linear hiding (trace)


import           Control.Monad
import           Control.Monad.Random
import           Control.Parallel.Strategies

import           Debug.Trace

import           Orphans

import qualified Numeric.LinearAlgebra as LA
import           Numeric.GSL.Integration
import           Numeric.GSL.Minimization
import           Numeric.FastMath

import           GHC.Conc (numCapabilities)

import           OlshausenOnStreams.Plotting
import           VanRossumError
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
    where errorFun v = reconstructionError patch phis (V.convert v)



--------------------------------------------------


{-oneIteration :: Int -> Patches Double -> Phis Double -> Phis Double-}
oneIteration patches phis = (meanPhis,errorInits,errorAs,errorPhis)
    where 
          --(scaledPhis,errorInits,errorAs,errorPhis) = V.unzip4 
          --           $ withStrategy (parTraversable rdeepseq)
          --           $ V.map (\patch -> oneIterationPatch patch phis) patches
          --numPatches = fromIntegral $ V.length patches
          --meanPhis   = V.map (S.map (\e -> e ^/ numPatches))
          --           . V.foldl1' (V.zipWith (S.zipWith (+))) 
          --           $ scaledPhis

          -- fitting the as (one a per phi per patch)
          initialAs  = V.replicate (V.length patches) $ S.replicate (V.length phis) 1
          fittedAs   = traceShowId 
                     $ withStrategy (parTraversable rdeepseq)
                     $ V.zipWith (\as patch -> gradientDescentToFindAs patch phis as) initialAs patches


          -- fit the phis
          fittedPhis :: V.Vector (Phis Double)
          fittedPhis = V.zipWith (\as patch -> updatePhis patch phis as) fittedAs patches

          -- scale as so that the maximum is not more than one
          -- **8 is there to "convince" the phi to go in the direction of
          -- one patch
          maxA       = V.maximum . V.map S.maximum $ fittedAs
          scaledAs   = traceShowId $ V.map (S.map (\a -> (a / maxA)**8)) fittedAs

          -- scale phis according to scaled as and learning rate
          eta        = 0.1
          scaledPhis :: V.Vector (Phis Double)
          scaledPhis = V.zipWith (\as phis' -> go (V.convert as) phis') scaledAs fittedPhis
              where go as phis' 
                     = V.zipWith3 (\a phi phi' -> S.zipWith (\e e' -> e + ((eta * a) *^ (e' - e))) phi phi') as phis phis'

          numPatches = fromIntegral $ V.length patches
          meanPhis   = V.map (S.map (\e -> e ^/ numPatches))
                     . V.foldl1' (V.zipWith (S.zipWith (+))) 
                     $ scaledPhis

          errorInits = traceShowId $ V.zipWith (\patch as -> reconstructionError patch phis as) patches initialAs
          errorAs    = traceShowId $ V.zipWith (\patch as -> reconstructionError patch phis as) patches fittedAs
          errorPhis  = traceShowId $ V.zipWith (\patch as -> reconstructionError patch meanPhis as) patches scaledAs




oneIterationPatch ::
  Patch Double
  -> Phis Double
  -> (Phis Double, Double, Double, Double)
oneIterationPatch patch phis = (scaledPhis, errorInit, errorAs, errorPhis)
    where initialAs = S.replicate (V.length phis) 1

          fittedAs = gradientDescentToFindAs patch phis initialAs
          fittedPhis = updatePhis patch phis fittedAs

          {-clampedAs  = traceShowId $ S.map (\a -> min 1 (max 0 a)) fittedAs-}
          clampedAs  = traceShowId $ S.map (\a -> a / 5) fittedAs
          minA       = S.minimum fittedAs
          maxA       = S.maximum fittedAs
          scaledPhis = V.zipWith3 (\a -> S.zipWith (\e e' -> e + ((0.1 * a) *^ (e' - e)))) 
                                  (V.convert clampedAs) phis fittedPhis 

          errorInit = traceShowId $ reconstructionError patch phis initialAs
          errorAs   = traceShowId $ reconstructionError patch phis fittedAs
          errorPhis = traceShowId $ reconstructionError patch fittedPhis fittedAs

          {-msg = printf "errors -> pre: %f, as: %f, phis: %f" errorInit errorAs errorPhis-}




testData = do
    patchA <- S.replicateM 32 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 0.0) :: IO (Patch Double)
    patchB <- S.replicateM 32 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 1.0) :: IO (Patch Double)
    let patches = V.fromList [patchA, patchB]

    phis  <- V.replicateM 2 $ S.replicateM 16 
                            $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> getRandomR (0,1)) :: IO (Phis Double)

    return (patches,phis)

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

savePatches :: FilePath -> Patches Double -> IO ()
savePatches fn patches = encodeFile fn (V.toList . V.map S.toList $ patches)
loadPatches :: FilePath -> IO (Patches Double)
loadPatches fn = V.map S.fromList . V.fromList <$> decodeFile fn

savePhis :: FilePath -> Phis Double -> IO ()
savePhis fn phis = encodeFile fn (V.toList . V.map S.toList $ phis)
loadPhis :: FilePath -> IO (Phis Double)
loadPhis fn = V.map S.fromList . V.fromList <$> decodeFile fn

loadDataset dn = do
    patch <- loadPatches (dn ++ "/patches.bin")
    phiNames <- init . sort . filter ("phis" `isPrefixOf`) <$> getDirectoryContents dn
    phis <- mapM (\n -> loadPhis $ dn ++ "/" ++ n) phiNames

    return (patch,phis)



-------------- U T I L I T Y ------------------

mergeSpikes :: S.Storable a => V.Vector (S.Vector a) -> S.Vector a
mergeSpikes = V.foldl' (S.++) S.empty


-- | add a coefficient to a spike
addA a (V3 x y z) = V4 a x y z
{-# INLINABLE addA #-}
-- | add the same coefficient to a range of spikes
addAs a vs = addA a <$> vs
{-# INLINABLE addAs #-}
-- | 'addAs' specialized for Storable Vectors
addAsS a vs = S.map (addA a) vs
{-# INLINABLE addAsS #-}

-- | create 'V3' from 'Vector'
-- no checks are performed to guarantee that 'v' is of the correct size
unsafePackV3 v = V3 (v S.! 0) (v S.! 1) (v S.! 2)
{-# INLINABLE unsafePackV3 #-}

unpackV3 v = S.fromListN 3 $ toList v
{-# INLINABLE unpackV3 #-}


infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x

infixl 4 <$$$>
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
f <$$$> x = fmap (fmap (fmap f)) x
