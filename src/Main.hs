
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

    let tests = [--testPatch 1 1 False
                {-,testPatch 1 2 False-}
                {-,testPatch 2 1 False-}
                {-,testPatch 2 2 False-}
                {- testPatch 3 1 False-}
                {-,testPatch 3 2 False-}
                {-,testPatch 3 3 False-}
                {-,testPatchR 3 1 False-}
                {-,testPatchR 3 2 False-}
                {-,testPatchR 3 3 False-}

                {-,testPatch1 1 True-}
                {-,testPatch1 2 True-}
                {-,testPatch2 1 True-}
                {-,testPatch2 2 True-}
                {-,testPatchR 3 1 True-}
                {-,testPatchR 3 2 True-}

                  testRealStuff 4 2
                ]

    -- execute and wait for results
    asyncs <- sequence $ map async tests
    sequence_ $ map wait asyncs


    putStrLn "done"



---------------
-- T E S T S --
---------------

iterations = 500

runTest :: IO (Patches Double) -> Phis Double -> Bool -> IO ()
runTest getPatches initialPhis random = do

    -- get patches once
    staticPatches <- getPatches
    let numPatches = V.length staticPatches

    -- create directory to store output
    let dn = printf "data/test-patch-%d-phi-%d-random-%s/" numPatches (V.length initialPhis) (show random)
    createDirectoryIfMissing True dn
    createDirectoryIfMissing True (dn ++ "phis/")
    createDirectoryIfMissing True (dn ++ "last-phis/")
    writeFile (dn ++ "errors.csv") ""


    -- run interations
    phisPtr <- newIORef initialPhis
    forM_ ([0..iterations]::[Int]) $ \i -> do

      patches <- if random then getPatches else return staticPatches

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


testPatch numPatches numPhi random = do

    initialPhis  <- V.replicateM numPhi $ S.replicateM 16
                                         $ (V3 <$> getRandomR (0,5)
                                               <*> getRandomR (0,5)
                                               <*> getRandomR (0,5)) :: IO (Phis Double)

    let patches = V.sequence $ V.fromListN numPatches [planeS (V3 2.5 2.5 2.5) (V3 1 0 0) 64
                                                      ,planeS (V3 2.5 2.5 2.5) (V3 0 1 0) 64
                                                      ,planeS (V3 2.5 2.5 2.5) (V3 0 0 1) 64
                                                      ]

    runTest patches initialPhis random


testRealStuff numPatches numPhis = do

    let ws = V3 5 5 0.1 -- window size
        patchSize = 32 -- minimum
        phiSize   = 16
    es <- convertToV3s <$> DVS.mmapDVSData "../common/ori-dir-stimulus-slice.aedat"

    initialPhis  <- V.replicateM numPhis
                  $ S.replicateM phiSize (mapM (\a -> getRandomR (0,a)) ws) :: IO (Phis Double)

    let getPatches = normalizePatches <$> selectPatches patchSize ws numPatches es

    runTest getPatches initialPhis True





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

    -- write phis in a format that gnuplot understands
    iforM_ phis' $ \phiI phi -> do
        let s = unlines . map (\(V3 x y z) -> printf "%f %f %f" x y z) . S.toList $ phi
        writeFile (printf "%sphis/it-%05d-phi-%02d.csv" dn i phiI) s
        writeFile (printf "%slast-phis/phi-%02d.csv" dn phiI) s


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
selectPatch s@(V3 sx sy st) es = do
  {-x <- getRandomR (0, 128-sx)-}
  {-y <- getRandomR (0, 128-sy)-}
  {-t <- getRandomR (view _t $ S.head es, (view _t $ S.last es) - st)-}

  {-[>traceM $ "(" ++ show x ++ "," ++ show y ++ "," ++ show t ++ ")"<]-}

  {-return $ sliceSpaceTimeWindow (V3 x y t) (V3 (x+sx) (y+sy) (t+st)) es-}


  i <- getRandomR (0, S.length es - 1)

  let m = es S.! i

  return $ sliceSpaceTimeWindow (m - (s/2)) (m + (s/2)) es




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


