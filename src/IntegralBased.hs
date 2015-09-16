

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
import           Linear


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

type Event a  = V3 a
type Events a = S.Vector (Event a)

type Patch a = Events a
type Phi a   = Events a
type PushV a = Events a

type As a    = S.Vector a

type Patches a = V.Vector (Patch a)
type Phis a    = V.Vector (Phi a)
type PushVs a  = V.Vector (PushV a)


-- | this function calculates a set of coefficients that
-- | scales the given phis to match the given patch best
-- | this is done by a gradient descent over the error
-- | function below
gradientDescentToFindAs :: Patch Double -> Phis Double -> As Double -> S.Vector Double
gradientDescentToFindAs patch phis randomAs = fst $ gradientDescentToFindAs' patch phis randomAs

gradientDescentToFindAs' :: Patch Double -> Phis Double -> As Double -> (S.Vector Double, LA.Matrix Double)
gradientDescentToFindAs' patch phis randomAs = minimizeV NMSimplex2 10e-9 1000 (S.replicate (length phis) 1) (\v -> asError patch phis (V.convert v)) (V.convert randomAs)

-- | distance between several spike trains
asError :: Patch Double -> Phis Double -> As Double -> Double
asError patch phis as = errorIntegral (phis' S.++ patches')
    where phis'    = V.foldl' (S.++) S.empty  $ V.zipWith (\a phi -> addAsS a phi) (V.convert as) phis
          patches' = addAsS (-1) patch

-- | find closest spike in the gradient field spanned by the patches
findClosestPatchSpike' :: Patch Double -> S.Vector Double -> (LA.Vector Double, LA.Matrix Double)
findClosestPatchSpike' patch v = minimizeV NMSimplex2 1e-6 1000 (S.replicate 3 1) go v
    where go = errFun ps . unsafePackV3
          ps = addAsS (-1) $ patch

findClosestPatchSpike :: Patch Double -> V3 Double -> V3 Double
findClosestPatchSpike patch v = unsafePackV3 $ fst $ findClosestPatchSpike' patch $ unpackV3 v


-- | TODO maybe fix this
-- findClosestPatchSpikeD :: Patches Double -> V3 Double -> (S.Vector Double, LA.Matrix Double)
-- findClosestPatchSpikeD patches v = minimizeVD VectorBFGS2 1e-6 1000 1 0.1 goE goD (unpackV3 v)
--     where goE = errFun ps . unsafePackV3
--           goD = unpackV3 . realDerivates ps . unsafePackV3
--           ps = V.toList $ addAs (-1) $ mergeSpikes patches


--------------------------------------------------

applyAntiGravity :: (Epsilon a, Floating a, S.Storable a) => Phis a -> Phis a
applyAntiGravity phis = V.map (S.map (\e -> e + go e)) phis
  where go a = V.foldl' (\acc p -> acc + S.foldl' (\acc2 b -> acc2 + antiGravForce a b) 0 p) 0 phis
  
antiGravForce a b = if nearZero dir then 0 else fdir
    where dir  = a - b
          ndir = normalize dir
          fdir = (0.001 / norm dir) *^ ndir


--------------------------------------------------


oneIteration :: Patches Double -> Phis Double -> Phis Double
oneIteration patches phis = V.zipWith (S.zipWith (+)) phis pushVs

    where pushVs = -- applyAntiGravity
                   collapsePushVectors
                 . withStrategy (parTraversable rdeepseq)
                 . V.map (\patch -> oneIterationPatch patch phis) 
                 $ patches

{-pushVector :: (Epsilon a, Floating a) => (V3 a -> V3 a) -> Phi a -> a -> V.Vector (V3 a)-}
{-pushVector dPatch phi fittedA = V.map (\e -> fittedA *^ normalize (dPatch e)) phi-}

pushVectors :: Patch Double -> Phis Double -> As Double  -> PushVs Double
pushVectors patch phis as = V.zipWith (\a -> S.map (\e -> (a*eta) *^ (findSpike e - e))) (V.convert as) phis
    where findSpike = findClosestPatchSpike patch
          eta       = 0.01

collapsePushVectors :: V.Vector (PushVs Double) -> PushVs Double
collapsePushVectors vs = V.foldl1' (V.zipWith (S.zipWith (\a b -> a + (b/n)))) vs
    where n = fromIntegral $ length vs

oneIterationPatch :: Patch Double -> Phis Double -> PushVs Double
oneIterationPatch patch phis = pushVectors patch phis (V.convert fittedAs)
    where -- find best as
          fittedAs = gradientDescentToFindAs (V.convert patch) phis (S.replicate (V.length phis) 1)




testData = do
    patchA <- S.replicateM 32 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 0.0) :: IO (Patch Double)
    patchB <- S.replicateM 32 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 1.0) :: IO (Patch Double)
    let patches = V.fromList [patchA, patchB]

    phis  <- V.replicateM 2 $ S.replicateM 16 
                            $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> getRandomR (0,1)) :: IO (Phis Double)

    return (patches,phis)

test = do

    (patches,phis) <- testData


    let phis' = iterate (oneIteration (patches)) phis



    t <- formatTime defaultTimeLocale "%F_%T" <$> getCurrentTime
    let dn = "data/integration_based_" ++ t ++ "/" 
    createDirectoryIfMissing True dn

    savePatches (dn ++ "patches.bin") patches


    forM_ (zip [0::Int ..] phis') $ \ (i,phi) -> do
      putStr $ "running iteration " ++ show i ++ " ... "
      tStart <- getCurrentTime
      encodeFile (dn ++ printf "phis_%05d.bin" i) (V.toList . V.map S.toList $ phi)
      tEnd <- getCurrentTime

      putStrLn $ "took " ++ show (tEnd .-. tStart)




    return (patches,phis,phis')

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
