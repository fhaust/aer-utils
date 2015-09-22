
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           IntegralBased
import           ArtificialData
import           Types
import           Linear

import           System.Directory
import           System.Locale
import           System.Environment

import           Text.Printf

import qualified Data.AER.DVS128 as DVS
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S

import           Data.Thyme.Clock
import           Data.Thyme.Format
import           Data.Maybe
import           Data.Function
import           Data.AffineSpace
import           Data.IORef

import           OlshausenOnStreams.Plotting

import           Control.Monad
import           Control.Monad.Random
import           Control.Lens
import           Control.Concurrent
import           Control.Concurrent.Async

import           Debug.Trace

_t = _z

main :: IO ()
main = do
    putStrLn "starting"

    testPatch1 1
    testPatch1 2
    testPatch2 1
    testPatch2 2
    testPatchR 3 1
    testPatchR 3 2
    testPatchR 3 3

    testPatchR1 1
    testPatchR1 2
    testPatchR2 1
    testPatchR2 2
    testPatchRR 3 1
    testPatchRR 3 2
    testPatchRR 3 3

    putStrLn "done"



---------------
-- T E S T S --
---------------

iterations = 500

runTest :: IO (Patches Double) -> Phis Double -> IO ()
runTest getPatches initialPhis = do

    -- create directory to store output
    numPatches <- V.length <$> getPatches
    let dn = printf "data/test-patch-%d-phi-%d/" numPatches (V.length initialPhis)
    createDirectoryIfMissing True dn
    writeFile (dn ++ "errors.csv") ""

    -- run interations
    phisPtr <- newIORef initialPhis
    forM_ ([0..iterations]::[Int]) $ \i -> do

      patches <- getPatches

      putStrLn $ "-- iteration " ++ show i ++ " --"
      sTime <- getCurrentTime 
      putStrLn $ "starttime: " ++ show sTime

      -- run iteration
      phis <- readIORef phisPtr
      let (phis',errorInit,errorA,errorPhi) = oneIteration patches phis
      writeIORef phisPtr phis'

      -- write out everything
      writeIteration dn i patches phis phis' errorInit errorA errorPhi


      eTime <- getCurrentTime
      putStrLn $ "time taken: " ++ show (eTime .-. sTime)

planeS o n num = S.fromList <$> plane o n num
randomPlaneS num = S.fromList <$> randomPlane num


testPatch1 numPhi = do

    initialPhis  <- V.replicateM numPhi $ S.replicateM 16
                                         $ (V3 <$> getRandomR (0,5)
                                               <*> getRandomR (0,5)
                                               <*> getRandomR (0,5)) :: IO (Phis Double)

    patches <- V.replicateM 1 $ planeS (V3 2.5 2.5 2.5) (V3 0 0 1) 64

    runTest (return patches) initialPhis

testPatch2 numPhi = do

    initialPhis  <- V.replicateM numPhi $ S.replicateM 16
                                         $ (V3 <$> getRandomR (0,5)
                                               <*> getRandomR (0,5)
                                               <*> getRandomR (0,5)) :: IO (Phis Double)

    patches <- V.sequence $ V.fromList [planeS (V3 2.5 2.5 2.5) (V3 0 0 1) 64
                                       ,planeS (V3 2.5 2.5 2.5) (V3 1 0 0) 64
                                       ]

    runTest (return patches) initialPhis


testPatchR numPatch numPhi = do

    initialPhis  <- V.replicateM numPhi $ S.replicateM 16
                                         $ (V3 <$> getRandomR (0,5)
                                               <*> getRandomR (0,5)
                                               <*> getRandomR (0,5)) :: IO (Phis Double)

    patches <- V.replicateM numPatch $ randomPlaneS 64

    runTest (return patches) initialPhis

-----------------------------
-- R A N D O M   T E S T S --
-----------------------------

testPatchR1 numPhi = do

    initialPhis  <- V.replicateM numPhi $ S.replicateM 16
                                         $ (V3 <$> getRandomR (0,5)
                                               <*> getRandomR (0,5)
                                               <*> getRandomR (0,5)) :: IO (Phis Double)

    let patches = V.replicateM 1 $ planeS (V3 2.5 2.5 2.5) (V3 0 0 1) 32

    runTest patches initialPhis


testPatchR2 numPhi = do

    initialPhis  <- V.replicateM numPhi $ S.replicateM 16
                                         $ (V3 <$> getRandomR (0,5)
                                               <*> getRandomR (0,5)
                                               <*> getRandomR (0,5)) :: IO (Phis Double)

    let patches = V.sequence $ V.fromList [planeS (V3 2.5 2.5 2.5) (V3 0 0 1) 32
                                          ,planeS (V3 2.5 2.5 2.5) (V3 1 0 0) 32
                                          ]

    runTest patches initialPhis

testPatchRR numPatch numPhi = do

    initialPhis  <- V.replicateM numPhi $ S.replicateM 16
                                         $ (V3 <$> getRandomR (0,5)
                                               <*> getRandomR (0,5)
                                               <*> getRandomR (0,5)) :: IO (Phis Double)

    let patches = V.replicateM numPatch $ randomPlaneS 32

    runTest patches initialPhis



writeIteration dn i patches phis phis' errorInit errorAs errorPhis = do
    savePatches (printf "%spatches%05d.bin" dn i) patches
    savePhis    (printf "%sphis%05d.bin" dn i) phis'
    _ <-multiplotFileS (printf "%sit-%05d.png" dn i) phis'
    _ <-multiplotFileS (printf "%sit-p-%05d.png" dn i) (patches V.++ phis')
    _ <-multiplotFileS (printf "%sit-step-%05d.png" dn i) (phis V.++ phis')
  
    -- process errors
    let (il,ih,is) = V.foldl' (\(l,h,s) x -> (min l x, max h x, s + x)) (1/0,-1/0,0) errorInit
    let (al,ah,as) = V.foldl' (\(l,h,s) x -> (min l x, max h x, s + x)) (1/0,-1/0,0) errorAs
    let (pl,ph,ps) = V.foldl' (\(l,h,s) x -> (min l x, max h x, s + x)) (1/0,-1/0,0) errorPhis
        im         = is / (fromIntegral $ V.length errorInit)
        am         = as / (fromIntegral $ V.length errorAs)
        pm         = ps / (fromIntegral $ V.length errorPhis)

    appendFile (printf "%serrors.csv" dn) (printf "%d %f %f %f %f %f %f %f %f %f\n" i im il ih am al ah pm pl ph)
    

readIteration dn i = do
    patches <- loadPatches (printf "%spatches%05d.bin" dn i)
    phis    <- loadPhis    (printf "%sphis%05d.bin" dn i)
    {-(errorInit, errorAs, errorPhis) <- read <$> readFile (printf "%serrors%05d.bin" dn i)-}
    return (patches, phis)




normalizePatches :: Patches Double -> Patches Double
normalizePatches = V.map normalizePatch

normalizePatch :: Patch Double -> Patch Double
normalizePatch patch = S.map (\(V3 x y t) -> V3 (x-minX) (y-minY) (t-minT)) patch
  where mins = S.foldl1' (\(V3 mx my mt) (V3 x y t) -> V3 (min mx x) (min my y) (min mt t)) 
        (V3 minX minY minT) = mins patch



selectPatches minSize windowSize num es = go V.empty
  where go ps | V.length ps >= num = return ps
              | otherwise = do
                  p <- selectPatch windowSize es
                  if S.length p < minSize then go ps
                                          else go (p `V.cons` ps)


selectPatch :: MonadRandom m => V3 Double -> S.Vector (V3 Double) -> m (S.Vector (V3 Double))
selectPatch (V3 sx sy st) es = do
  x <- getRandomR (0, 128-sx)
  y <- getRandomR (0, 128-sy)
  t <- getRandomR (view _t $ S.head es, (view _t $ S.last es) - st)

  {-traceM $ "(" ++ show x ++ "," ++ show y ++ "," ++ show t ++ ")"-}

  return $ sliceSpaceTimeWindow (V3 x y t) (V3 (x+sx) (y+sy) (t+st)) es




convertToV3 (DVS.Event (DVS.Address DVS.U x y) t) = Just (V3 (fromIntegral x) (fromIntegral y) (toSeconds t))
convertToV3 _                                     = Nothing

convertToV3s :: S.Vector (DVS.Event DVS.Address) -> S.Vector (V3 Double)
convertToV3s = S.fromList . catMaybes . map convertToV3 . S.toList

sliceSpaceTimeWindow (V3 lx ly lt) (V3 hx hy ht) = S.filter go . sliceTimeWindow lt ht
  where go (V3 x y t) =  x >= lx && x <= hx
                      && y >= ly && y <= hy

sliceTimeWindow :: Double -> Double -> S.Vector (V3 Double) -> S.Vector (V3 Double)
sliceTimeWindow lt ht es = S.slice (li) (hi - li) es
    where bs x = binarySearch (compare `on` view _t) es (V3 0 0 x)
          li   = bs lt
          hi   = bs ht

binarySearch cmp vec e = binarySearchByBounds cmp vec e 0 (S.length vec - 1)

binarySearchByBounds cmp vec e = loop
 where
 loop !l !u
   | u <= l    = l
   | otherwise = let e' = S.unsafeIndex vec k
                 in case cmp e' e of
                      LT -> loop (k+1) u
                      EQ -> k
                      GT -> loop l     k
  where k = (u + l) `div` 2


