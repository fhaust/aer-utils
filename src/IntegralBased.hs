

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

import           GHC.Conc (numCapabilities)


type Event a  = V3 a
type Events a = V.Vector (Event a)

type Patch a = Events a
type Phi a   = Events a
type As a    = V.Vector a

type Patches a = V.Vector (Patch a)
type Phis a    = V.Vector (Phi a)


-- | this function calculates a set of coefficients that
-- | scales the given phis to match the given patch best
-- | this is done by a gradient descent over the error
-- | function below
gradientDescentToFindAs :: Patch Double -> Phis Double -> As Double -> S.Vector Double
gradientDescentToFindAs patch phis randomAs = fst $ minimizeV NMSimplex2 10e-9 1000 (S.replicate (length phis) 1) (\v -> asError patch phis (V.convert v)) (V.convert randomAs)

-- | distance between several spike trains
asError :: Patch Double -> Phis Double -> As Double -> Double
asError patch phis coeffs = realIntegral' (V.toList (phis' V.++ patches')) (-1000) 1000 --(V3 0 0 (-10)) (V3 128 128 10)
    where phis'    = foldl (V.++) V.empty  $ V.zipWith (\a phi -> addAs a phi) coeffs phis
          patches' = addAs (-1) patch

-- | find closest spike in the gradient field spanned by the patches
{-findClosestPatchSpike' :: Patch Double -> _ -> (_, LA.Matrix Double)-}
findClosestPatchSpike' patch v = minimizeV NMSimplex2 1e-6 1000 (S.replicate 3 1) go v
    where go = errFun ps . unsafePackV3
          ps = V.toList $ addAs (-1) $ patch

findClosestPatchSpike :: Patch Double -> V3 Double -> V3 Double
findClosestPatchSpike patch v = unsafePackV3 $ fst $ findClosestPatchSpike' patch $ unpackV3 v


-- | TODO maybe fix this
-- findClosestPatchSpikeD :: Patches Double -> V3 Double -> (S.Vector Double, LA.Matrix Double)
-- findClosestPatchSpikeD patches v = minimizeVD VectorBFGS2 1e-6 1000 1 0.1 goE goD (unpackV3 v)
--     where goE = errFun ps . unsafePackV3
--           goD = unpackV3 . realDerivates ps . unsafePackV3
--           ps = V.toList $ addAs (-1) $ mergeSpikes patches


--------------------------------------------------

applyAntiGravity :: (Epsilon a, Floating a) => Phis a -> Phis a
applyAntiGravity phis = G.map (G.map (\e -> e + go e)) phis
  where go a = G.foldl' (\acc p -> acc + G.foldl' (\acc2 b -> acc2 + antiGravForce a b) 0 p) 0 phis
  
antiGravForce a b = if nearZero dir then 0 else fdir
    where dir  = a - b
          ndir = normalize dir
          fdir = (1 / quadrance dir) *^ ndir


--------------------------------------------------


oneIteration :: Patches Double -> Phis Double -> Phis Double
oneIteration patches phis = V.zipWith (V.zipWith (+)) phis pushVs

    where pushVs = collapsePushVectors
                 . withStrategy (parTraversable rdeepseq)
                 . V.map (\patch -> oneIterationPatch patch phis) 
                 $ patches

{-pushVector :: (Epsilon a, Floating a) => (V3 a -> V3 a) -> Phi a -> a -> V.Vector (V3 a)-}
{-pushVector dPatch phi fittedA = V.map (\e -> fittedA *^ normalize (dPatch e)) phi-}

pushVectors :: Patch Double -> Phis Double -> As Double  -> V.Vector (V.Vector (V3 Double))
pushVectors patch phis as = V.zipWith (\a -> V.map (\e -> a *^ (findSpike e - e))) as phis
    where findSpike = findClosestPatchSpike patch

collapsePushVectors :: Fractional a => V.Vector (V.Vector (V.Vector (V3 a))) -> V.Vector (V.Vector (V3 a))
collapsePushVectors vs = V.foldl1' (V.zipWith (V.zipWith (\a b -> a + (b/n)))) vs
    where n = fromIntegral $ length vs

oneIterationPatch :: Patch Double -> Phis Double -> V.Vector (V.Vector (V3 Double))
oneIterationPatch patch phis = pushVectors patch phis (V.convert fittedAs)
    where -- find best as
          fittedAs = gradientDescentToFindAs (V.convert patch) phis (V.replicate (V.length phis) 1)


gauss :: V4 Double -> Double
gauss (V4 a b c d) = a * exp( -0.5 * (b**2+c**2+d**2) )
errFun :: [V4 Double] -> V3 Double -> Double
errFun gs (V3 x y z) = sum [ gauss (V4 a (x-b) (y-c) (z-d)) | (V4 a b c d) <- gs ]
squaredErrFun :: [V4 Double] -> V3 Double -> Double
squaredErrFun gs v = (errFun gs v)**2

-- | numeric integration, mostly for comparison
intFun :: [V4 Double] -> (Double,Double)
intFun gs = integrateQAGI 1e-9 1000 (\z -> fst $ integrateQAGI 1e-9 1000 (\y -> fst $ integrateQAGI 1e-9 1000 (\x -> squaredErrFun gs (V3 x y z)) ))

{-realIntegral :: [V4 Double] -> Double-}
{-realIntegral vs = (2*pi)**(3/2) * sum [ a**2 | (V4 a _ _ _) <- vs ]-}
{-                +  2*pi**(3/2)  * sum [ sum [ ai * aj * g (bi-bj) (ci-cj) (di-dj) | (V4 aj bj cj dj) <- is] | ((V4 ai bi ci di):is) <- tails vs ]-}
{-    where g a b c = exp ( -0.25 * (a**2+b**2+c**2))-}

realIntegral' vs (V3 lx ly lz) (V3 hx hy hz) = indefIntegral hx hy hz  - indefIntegral hx hy lz  - indefIntegral hx ly hz  + indefIntegral hx ly lz  
                                             - indefIntegral lx hy hz  + indefIntegral lx hy lz  + indefIntegral lx ly hz  - indefIntegral lx ly lz 
    where indefIntegral x y z = realIntegral vs (V3 x y z)

-- | the integral as calculated by hand (and SAGE)
realIntegralOld vs (V3 x y z) = foo + bar
    where foo = 1/8*pi**(3/2) * sum [ a**2 * erf(x - b) * erf(y - c) * erf(z - d) | (V4 a b c d) <- vs ]
          bar = 1/4*pi**(3/2) * sum [ sum [ aj*ai * erf(x - (bj+bi)/2) * erf(y - (cj+ci)/2) * erf(z - (dj+di)/2) * exp( - 1/4*(bi-bj)**2 - 1/4*(ci-cj)**2  - 1/4*(di-dj)**2) | (V4 aj bj cj dj) <- is ] | ((V4 ai bi ci di):is) <- tails vs ]

-- | the integral "optimized"
realIntegral vs (V3 x y z) = foo
    where foo = 1/8*pi**(3/2) * (sum.parM) [ fooInner vi + 2 * sum [ barInner vi vj | vj <- js ] | (vi:js) <- tails vs ]
          fooInner (V4 a b c d) = a**2 * erf(x - b) * erf(y - c) * erf(z - d)
          barInner (V4 ai bi ci di) (V4 aj bj cj dj)
            = aj*ai * erf(x - (bj+bi)/2) * erf(y - (cj+ci)/2) * erf(z - (dj+di)/2)
            * exp( - 1/4 * ((bi-bj)**2 + (ci-cj)**2 + (di-dj)) )
          parM = withOptStrat

withOptStrat ls | length ls < numCapabilities = ls
                | otherwise = withStrategy (parListChunk (length ls `div` numCapabilities) rdeepseq) ls


-- derivates of the error function

realDerivateX vs (V3 x y z) = -2 * foo * bar
  where foo = sum [ a * (x - b) * gauss (V4 a (x-b) (y-c) (z-d))  | (V4 a b c d) <- vs ]
        bar = sum [ gauss (V4 a (x-b) (y-c) (z-d)) | (V4 a b c d) <- vs ]
  
realDerivateY vs (V3 x y z) = -2 * foo * bar
  where foo = sum [ a * (y - c) * gauss (V4 a (x-b) (y-c) (z-d))  | (V4 a b c d) <- vs ]
        bar = sum [ gauss (V4 a (x-b) (y-c) (z-d)) | (V4 a b c d) <- vs ]

realDerivateZ vs (V3 x y z) = -2 * foo * bar
  where foo = sum [ a * (z - d) * gauss (V4 a (x-b) (y-c) (z-d))  | (V4 a b c d) <- vs ]
        bar = sum [ gauss (V4 a (x-b) (y-c) (z-d)) | (V4 a b c d) <- vs ]

realDerivates vs v = V3 (realDerivateX vs v) (realDerivateY vs v) (realDerivateZ vs v)





testData = do
    patchA <- V.replicateM 32 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 0.0) :: IO (Patch Double)
    patchB <- V.replicateM 32 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 1.0) :: IO (Patch Double)
    let patches = V.fromList [patchA, patchB]

    phis  <- V.replicateM 8 $ V.replicateM 16 
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
      encodeFile (dn ++ printf "phis_%05d.bin" i) (V.toList . V.map V.toList $ phi)
      tEnd <- getCurrentTime

      putStrLn $ "took " ++ show (tEnd .-. tStart)




    return (patches,phis,phis')

savePatches :: FilePath -> Patches Double -> IO ()
savePatches fn patches = encodeFile fn (V.toList . V.map V.toList $ patches)
loadPatches :: FilePath -> IO (Patches Double)
loadPatches fn = V.map V.fromList . V.fromList <$> decodeFile fn

savePhis :: FilePath -> Phis Double -> IO ()
savePhis fn phis = encodeFile fn (V.toList . V.map V.toList $ phis)
loadPhis :: FilePath -> IO (Phis Double)
loadPhis fn = V.map V.fromList . V.fromList <$> decodeFile fn

loadDataset dn = do
    patch <- loadPatches (dn ++ "/patches.bin")
    phiNames <- init . sort . filter ("phis" `isPrefixOf`) <$> getDirectoryContents dn
    phis <- mapM (\n -> loadPhis $ dn ++ "/" ++ n) phiNames

    return (patch,phis)

-------------- U T I L I T Y ------------------

mergeSpikes :: (G.Vector v0 (v1 a), G.Vector v1 a) => v0 (v1 a) -> v1 a
mergeSpikes = G.foldl' (G.++) G.empty


-- | add a coefficient to a spike
addA a (V3 x y z) = V4 a x y z
-- | add the same coefficient to a range of spikes
addAs a vs = addA a <$> vs
-- | 'addAs' specialized for Storable Vectors
addAsS a vs = S.map (addA a) vs

-- | create 'V3' from 'Vector'
-- no checks are performed to guarantee that 'v' is of the correct size
unsafePackV3 v = V3 (v G.! 0) (v G.! 1) (v G.! 2)

unpackV3 v = G.fromListN 3 $ toList v


infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x

infixl 4 <$$$>
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
f <$$$> x = fmap (fmap (fmap f)) x
