
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           IntegralBased
import           Linear

import           System.Directory
import           System.Locale

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

import           Debug.Trace

_t = _z

main :: IO ()
main = do
    putStrLn "starting"
    {-_ <- IntegralBased.test -}

    let n = 4 
        windowSize = V3 5 5 0.1 -- 5px * 5px * 100ms windows
        minSize    = 32          -- minimal number of elements in the window

    -- load dataset
    es <- convertToV3s <$> DVS.mmapDVSData "../aer/data/DVS128-citec-walk-fast.aedat"

    -- create initial random phis
    initialPhis  <- V.replicateM 2 $ V.replicateM 16 
                                   $ (V3 <$> getRandomR (0,view _x windowSize)
                                         <*> getRandomR (0,view _y windowSize)
                                         <*> getRandomR (0,view _t windowSize)) :: IO (Phis Double)


    -- create directory to store output
    t <- formatTime defaultTimeLocale "%F_%T" <$> getCurrentTime
    let dn = "data/integration_based_" ++ t ++ "/" 
    createDirectoryIfMissing True dn

    putStrLn "loaded dataset, now crunching"

    -- run interations
    phisPtr <- newIORef initialPhis
    guiPtr  <- newIORef Nothing
    forM_ ([0..]::[Int]) $ \i -> do

      putStrLn $ "-- iteration " ++ show i ++ " --"
      sTime <- getCurrentTime 
      putStrLn $ "starttime: " ++ show sTime

      -- select random patches
      patches <- normalizePatches <$> selectPatches minSize windowSize n es

      -- run iteration
      phis <- readIORef phisPtr
      let phis' = oneIteration patches phis
      writeIORef phisPtr phis'

      -- write out everything
      savePatches (printf "%spatches%05d.bin" dn i) patches
      savePhis    (printf "%sphis%05d.bin" dn i) phis'

      -- display results
      oldTid <- readIORef guiPtr
      case oldTid of
        (Just tid) -> killThread tid
        Nothing    -> return ()
      tid <- multiplotEventsAsync ({-V.toList patches ++-} V.toList phis)
      writeIORef guiPtr (Just tid)

      -- write out image
      _ <- multiplotFile (printf "%sit-%05d.png" dn i) (V.toList phis')

      eTime <- getCurrentTime
      putStrLn $ "time taken: " ++ show (eTime .-. sTime)


    putStrLn "done"


normalizePatches :: Patches Double -> Patches Double
normalizePatches = V.map normalizePatch

normalizePatch :: Patch Double -> Patch Double
normalizePatch patch = V.map (\(V3 x y t) -> V3 (x-minX) (y-minY) (t-minT)) patch
  where mins = V.foldl1' (\(V3 mx my mt) (V3 x y t) -> V3 (min mx x) (min my y) (min mt t)) 
        (V3 minX minY minT) = mins patch



selectPatches minSize windowSize num es = go V.empty
  where go ps | V.length ps >= num = return ps
              | otherwise = do
                  p <- selectPatch windowSize es
                  if length p < minSize then go ps
                                          else go (p `V.cons` ps)


selectPatch :: MonadRandom m => V3 Double -> S.Vector (V3 Double) -> m (V.Vector (V3 Double))
selectPatch (V3 sx sy st) es = do
  x <- getRandomR (0, 128-sx)
  y <- getRandomR (0, 128-sy)
  t <- getRandomR (view _t $ S.head es, (view _t $ S.last es) - st)

  {-traceM $ "(" ++ show x ++ "," ++ show y ++ "," ++ show t ++ ")"-}

  return $ V.convert $ sliceSpaceTimeWindow (V3 x y t) (V3 (x+sx) (y+sy) (t+st)) es




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


