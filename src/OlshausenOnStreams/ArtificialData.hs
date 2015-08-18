
{-# LANGUAGE ParallelListComp #-}

module OlshausenOnStreams.ArtificialData where




import qualified Data.AER.DVS128 as DVS

import           Linear

import           Data.Thyme.Clock

import           Control.Monad
import           Control.Monad.Random

import           Debug.Trace

movingEdge p dt = concat [ edge p x t | t <- [0,dt..] | x <- [0..127] ]



edge p x t = [ DVS.Event (DVS.Address p x y) t | y <- [0..127] ]


randomPlane num = do
    x <- getRandomR (0, 1)
    y <- getRandomR (0, 1)
    t <- getRandomR (0, 1)
    let o = V3 0 0 0
        n = normalize $ V3 x y t
        u = normalize $ perpendicular n
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

planePoint o n = do
    let u = normalize $ perpendicular n
        v = cross n u


    fu <- getRandom
    fv <- getRandom

    return $ o + (clampEvent $ fu *^ u) + (clampEvent $ fv *^ v)

clampEvent (V3 x y t) = V3 (clamp 0 127 x) (clamp 0 127 y) t
    where clamp mi ma x = min ma (max mi x)

perpendicular (V3 x y z) = V3 1 1 a
  where a = (0 - x - y) / z


perpendicular' v@(V3 x y z) | m == x    = V3 0 (-z) (y)
                            | m == y    = V3 (-z) 0 (x)
                            | otherwise = V3 (-y) (x) 0
    where m = minimum v
