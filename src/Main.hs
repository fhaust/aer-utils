
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           IntegralBased
import           ArtificialData
import           Types
import           Linear
import           ChartPlotting

import           System.Directory
import           System.Locale
import           System.Environment

import           Text.Printf

import qualified Data.AER.DVS128 as DVS
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S

import qualified Data.Text as T

import           Data.Thyme.Clock
import           Data.Thyme.Format
import           Data.Maybe
import           Data.Function
import           Data.AffineSpace
import           Data.IORef
import           Data.List
import           Data.Binary
import           Data.Foldable

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
                {-,testPatch 3 1 False-}
                {-,testPatch 3 2 False-}
                {-,testPatch 3 3 False-}

                {-,testRealStuff "../common/ori-dir-stimulus-slice.aedat" "grid" 9 2-}
                {-,testRealStuff "../common/ori-dir-stimulus-hor-line-slice.aedat" "horline" 9 2-}
                testRealStuff "../common/ori-dir-stimulus-hor-line-and-back.aedat" "horline-and-back" 9 2
                ] :: [IO ()]

    -- execute and wait for results
    asyncs <- mapM async tests
    mapM_ wait asyncs


    putStrLn "done"



---------------
-- T E S T S --
---------------

iterations = 500

runTest :: V3 Double -> String -> IO (Patches Double) -> Phis Double -> Bool -> IO ()
runTest windowSize prefix getPatches initialPhis random = do

    -- get patches once
    staticPatches <- getPatches
    let numPatches = V.length staticPatches

    -- create directory to store output
    let dn = prefix
    createDirectoryIfMissing True dn
    createDirectoryIfMissing True (dn ++ "phis/")
    createDirectoryIfMissing True (dn ++ "last-phis/")
    writeFile (dn ++ "errors.csv") ""

    -- setup metrics
    let labelBase = printf "patch-%d-phi-%d" numPatches (V.length initialPhis)
    let iLabel = T.pack (labelBase ++ "-iterations")
    let eLabel = T.pack (labelBase ++ "-fit-a-error")

    -- run interations
    phisPtr <- newIORef initialPhis
    err <- forM ([0..iterations]::[Int]) $ \i -> do

      patches <- if random then getPatches else return staticPatches

      putStrLn $ "-- iteration " ++ show i ++ " --"
      sTime <- getCurrentTime 
      putStrLn $ "starttime: " ++ show sTime

      -- run iteration
      phis <- readIORef phisPtr
      let (phis',errorInit,errorA,errorPhi) = oneIteration patches phis
      let clampE (V3 x y z) = V3 (min (windowSize^._x) (max 0 x))
                                 (min (windowSize^._y) (max 0 y))
                                 (min (windowSize^._z) (max 0 z))
          clampedPhis' = V.map (S.map clampE) phis'
      writeIORef phisPtr clampedPhis'

      -- write out everything
      writeIteration dn i patches phis phis' errorInit errorA errorPhi

      eTime <- getCurrentTime
      putStrLn $ "time taken: " ++ show (eTime .-. sTime)

      return (errorInit,errorA)


    -- plot errors
    let (ea,eb) = unzip err

    plotReconstructionError (dn ++ "errors.png") (map V.toList ea) (map V.toList eb)

    putStrLn "done"

planeS o n num = S.fromList <$> plane o n num
randomPlaneS num = S.fromList <$> randomPlane num


testPatch numFeatures numPatches numPhi random = do

    initialPhis  <- V.replicateM numPhi $ S.replicateM 16
                                          (V3 <$> getRandomR (0,5)
                                              <*> getRandomR (0,5)
                                              <*> getRandomR (0,5)) :: IO (Phis Double)

    let patches = take numFeatures [planeS (V3 2.5 2.5 2.5) (V3 1 0 1) 64
                                   ,planeS (V3 2.5 2.5 2.5) (V3 0 1 1) 64
                                   ,planeS (V3 2.5 2.5 2.5) (V3 1 0 (-1)) 64
                                   ,planeS (V3 2.5 2.5 2.5) (V3 0 1 (-1)) 64
                                   ]
    let getPatches = V.replicateM numPatches (join (uniform patches))
  
    let prefix = printf "data/test-patch-%d-phi-%d-random-%s/" numPatches numPhi (show random)
    runTest (V3 5 5 5) prefix getPatches initialPhis random


testRealStuff fn ident numPatches numPhis = do

    let ws = V3 5 5 0.1 -- window size
        patchSize = 32 -- minimum
        phiSize   = 16
    es <- convertToV3s <$> DVS.mmapDVSData fn

    initialPhis  <- V.replicateM numPhis
                  $ S.replicateM phiSize (mapM (\a -> getRandomR (0,a)) ws) :: IO (Phis Double)

    let getPatches = normalizePatches <$> selectPatches patchSize ws numPatches es

    let prefix = printf "data/test-patch-%d-phi-%d-fn-%s/" numPatches numPhis (ident::String)
    runTest ws prefix getPatches initialPhis True



writeIteration' :: FilePath -> Patches Double -> Phis Double -> IO ()
writeIteration' fn patches phis = encodeFile fn (toList patches, toList phis)

readIteration' :: FilePath -> IO (Patches Double, Phis Double)
readIteration' fn = do
    (patches,phis) <- decodeFile fn
    return (V.fromList patches, V.fromList phis)


writeIteration dn i patches phis phis' errorInit errorAs errorPhis = do
    writeIteration' (printf "%sit-%05d-data.bin"  dn i) patches phis'
    plotMoreEvents (printf "%sit-%05d-phis.png" dn i) phis'
    plotMoreEvents (printf "%sit-%05d-patches.png" dn i) patches


readIteration dn i = do
    (patches,phis) <- readIteration' (printf "%sit-%05d-data.bin" dn i)
    return (patches, phis)

readIterations dn = do
    fns <- sort . filter ("it-" `isPrefixOf`) <$> getDirectoryContents dn
    mapM (\fn -> readIteration' (dn ++ "/" ++ fn)) fns




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
selectPatch s es = do
  i <- getRandomR (0, S.length es - 1)
  let m = es S.! i
  return $ sliceSpaceTimeWindow (m - (s/2)) (m + (s/2)) es




convertToV3 (DVS.Event (DVS.Address DVS.U x y) t) = Just (V3 (fromIntegral x) (fromIntegral y) (toSeconds t))
convertToV3 _                                     = Nothing

convertToV3s :: S.Vector (DVS.Event DVS.Address) -> S.Vector (V3 Double)
convertToV3s = S.fromList . mapMaybe convertToV3 . S.toList

sliceSpaceTimeWindow (V3 lx ly lt) (V3 hx hy ht) = S.filter go . sliceTimeWindow lt ht
  where go (V3 x y t) =  x >= lx && x <= hx
                      && y >= ly && y <= hy

sliceTimeWindow :: Double -> Double -> S.Vector (V3 Double) -> S.Vector (V3 Double)
sliceTimeWindow lt ht es = S.slice li (hi - li) es
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
