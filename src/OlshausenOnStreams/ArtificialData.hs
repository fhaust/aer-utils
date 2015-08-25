
{-# LANGUAGE ParallelListComp #-}

module OlshausenOnStreams.ArtificialData where




import qualified Data.AER.DVS128 as DVS

import           Linear

import           Data.Thyme.Clock
import           Data.Word7

import           Control.Monad
import           Control.Monad.Random

import           Debug.Trace

movingEdge ::
  DVS.Polarity
  -> NominalDiffTime
  -> [DVS.Event DVS.Address]
movingEdge p dt = concat [ edge p x t | t <- [0,dt..] | x <- [0..127] ]


edge :: 
  DVS.Polarity
  -> Word7 -> NominalDiffTime -> [DVS.Event DVS.Address]
edge p x t = [ DVS.Event (DVS.Address p x y) t | y <- [0..127] ]

randomPlane ::
  (Floating a, Ord a, Show a, Epsilon a, Random a, MonadRandom m) =>
  Int -> m [V4 a]
randomPlane num = do
    x <- getRandomR (0, 1)
    y <- getRandomR (0, 1)
    t <- getRandomR (0, 1)
    let o = V3 0 0 0
        n = normalize $ V3 x y t

    plane o n num

plane :: 
  (Floating a, Ord a, Show a, Epsilon a, Random a, MonadRandom m) =>
  V3 a -> V3 a -> Int -> m [V4 a]
plane o n num = do
    let u = normalize $ perpendicular n
        v = cross n u

    traceM $ "o = " ++ show o
    traceM $ "n = " ++ show n
    traceM $ "u = " ++ show u
    traceM $ "v = " ++ show v


    ps <- replicateM num $ do
      fu <- getRandom
      fv <- getRandom
      return $ o + (fu *^ u) + (fv *^ v)

    return $ map (\(V3 a b c) -> (V4 a b c 1)) ps

drawPlanePoint ::
  (Floating a, Ord a, Epsilon a, Random a, MonadRandom m) =>
  V3 a -> V3 a -> m (V3 a)
drawPlanePoint o n = do
    let u = normalize $ perpendicular n
        v = cross n u


    fu <- getRandom
    fv <- getRandom

    return $ o + clampEvent (fu *^ u) + clampEvent (fv *^ v)

clampEvent :: (Num a, Ord a) => V3 a -> V3 a
clampEvent (V3 x y t) = V3 (clamp 0 127 x) (clamp 0 127 y) t
    where clamp mi ma a = min ma (max mi a)

perpendicular :: (Num a, Ord a) => V3 a -> V3 a
perpendicular v@(V3 x y z) | m == x    = V3 0 (-z) y
                           | m == y    = V3 (-z) 0 x
                           | otherwise = V3 (-y) x 0
    where m = minimum v
