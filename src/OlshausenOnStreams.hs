
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module OlshausenOnStreams where


import qualified Data.AER.DVS128 as DVS


import           Control.Monad.Random
import           Control.Monad
import           Control.Parallel.Strategies

import           Data.Thyme.Clock
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import qualified Data.Vector.Generic as G

import           Data.Function
import           Data.List
import           Data.Word7

import           Data.DTW

import           Data.List.Ordered

import           Numeric.AD


import           OlshausenOnStreams.ArtificialData
import           OlshausenOnStreams.Types
import qualified OlshausenOnStreams.AERPlot as Plot


fromDVSEvent :: Fractional a => DVS.Event DVS.Address -> Event a
fromDVSEvent (DVS.Event (DVS.Address p x y) t) = Event { time = toSeconds t
                                                       , posX = fromIntegral x
                                                       , posY = fromIntegral y
                                                       , val  = if p == DVS.U then 1 else -1
                                                       }

createRandomEvent :: (Num a, Random a, MonadRandom m) => m (Event a)
createRandomEvent = Event <$> getRandomR (0,1) <*> getRandomR (0,127) <*> getRandomR (0,127) <*> getRandomR (0,1)


-- | create random eventstream of length n
createRandomEvents :: (Ord a, Num a, Random a, MonadRandom m) => Int -> m (Events a)
createRandomEvents n = sortEvents <$> V.replicateM n createRandomEvent




pickRandomEvents :: (G.Vector v a, MonadRandom m) => Int -> v a -> m (v a)
pickRandomEvents n es = do
    offset <- getRandomR (0, G.length es - n - 1)
    return $ G.slice offset n es


-- | extract space time cubes from an event stream
-- TODO: this could be much more efficient by using the fact that the
-- streams are sorted on time, therefore doing a binary search on that
extractSTC ::
  NominalDiffTime
  -> NominalDiffTime
  -> Word7 -> Word7 -> Word7 -> Word7
  -> [DVS.Event DVS.Address]
  -> [DVS.Event DVS.Address]
extractSTC t0 t1 x0 x1 y0 y1 es = filter cond es
    where cond (DVS.qview -> (_,x,y,t)) = x >= x0 && x < x1 &&
                                          y >= y0 && y < y1 &&
                                          t >= t0 && t < t1


extractRandomSTC st sx sy es = do
    let h = head es
    let l = last es
    rt <- getRandomR (DVS.timestamp h, DVS.timestamp l - st)
    rx <- Word7 <$> getRandomR (0, 127 - unWord7 sx)
    ry <- Word7 <$> getRandomR (0, 127 - unWord7 sy)

    return $ extractSTC rt st rx sx ry sy es

reconstructEvents :: (Ord a, Num a) => [a] -> [Events a] -> Events a
reconstructEvents as φs = mergeEvents $ scaleP as φs
    where scaleP = zipWith (\a φ -> V.map (\e -> e { val = a * val e }) φ)


-- | sort events based on timestamp
sortEvents :: Ord a => Events a -> Events a
sortEvents = V.modify (V.sortBy (compare `on` time))

{-concatEvents :: Ord a => [Events a] -> Events a-}
{-concatEvents es = sortEvents $ V.concat es-}

-- | merge multiple sorted event streams
mergeEvents :: Ord a => G.Vector v (Event a) => [v (Event a)] -> v (Event a)
mergeEvents = G.fromList . mergeAllBy (compare `on` time) . map G.toList . sortHeads
    where sortHeads = sortBy (compare `on` (time . G.head))

-- | this is basically euclidian distance in space-time
eventDistance :: Floating a => Event a -> Event a -> a
eventDistance a b = sqrt (positionDistance + timeDistance + valDistance)
  where positionDistance = (posX a - posX b)**2 + (posY a - posY b)**2
        timeDistance     = (time a - time b)**2
        valDistance      = (val  a - val  b)**2




-- | super mega generic type 0o
eventDTW :: forall c (v :: * -> *) a.
            (Floating c, Ord c, G.Vector v a, G.Vector v Int, DataSet (v a), Item (v a) ~ Event c) =>
            v a -> v a -> Result c
eventDTW = fastDtw eventDistance reduceEventStream 1

-- | cut the eventstream in half by skipping every second event
reduceEventStream :: (G.Vector v a, G.Vector v Int) => v a -> v a
reduceEventStream v = G.backpermute v (G.fromList [0,2..G.length v - 1])

normalizeEventTimes ::
  (Fractional a, Ord a) => V.Vector (Event a) -> V.Vector (Event a)
normalizeEventTimes es = V.map fixT es
    where minT  = V.minimum . V.map time $ es
          maxT  = V.maximum . V.map time $ es
          fixT e = e { time = (time e - minT) / (maxT - minT) }

sparseness :: Floating a => a -> a -> a
sparseness σ a = s (a / σ) where s x = log (1+x*x)

logit x = log (1+x*x)

-- | TODO give parameters as ... parameters
errorFunction ::
  (Floating a, Ord a) => Events a -> [Events a] -> [a] -> a
errorFunction patch φs as = λ   * cost (eventDTW patch (reconstructEvents as φs))
                          + bos * sum [ sparseness σ a | a <- as ]
    where bos = 6.96
          λ   = 100
          σ   = 0.316 


doTheTimeWarp :: forall a. (Ord a, Num a, Floating a)
              => Events a -> [Events a] -> [a] -> [[a]]
doTheTimeWarp patch φs = gradientDescent go
  where go :: forall t. (Scalar t ~ a, Mode t, Floating t, Ord t) => [t] -> t
        go = errorFunction (fmap auto <$> patch) (fmap (fmap auto) <$> φs)

doTheTimeWarpAgain :: (Floating a, Ord a) 
                   => Events a -> [Events a] -> [a] -> [a]
doTheTimeWarpAgain patch φs = go . doTheTimeWarp patch φs
  where go (a:b:xs) | errDiff a b < 0.01 = b
                    | otherwise          = go (b:xs)
        errFun = errorFunction patch φs 
        errDiff a b = abs (errFun a - errFun b)

-- | calculate as per patch, outer list is per patch, inner is as 
fitAs :: MonadRandom m
      => [Patch Float] -> [Phi Float] -> m [[Float]]
fitAs patches φs = do

    let numPhis = length φs
        numPatches = length patches

    -- generate random as
    ass <- replicateM numPatches (replicateM numPhis getRandom)

    -- fit as to patches
    let fittedAs = [ doTheTimeWarpAgain patch φs as | patch <- patches | as <- ass ] 

    -- return results with parallel strategy
    return (withStrategy (parList rdeepseq) fittedAs)



-- | calculate the "vector" along which the phi has to be pushed to get
-- closer to the patch
getPushDirections ::
  (Floating t1, Ord t1) =>
  V.Vector (Event t1) -> V.Vector (Event t1) -> [Event t1]
getPushDirections φ patch = dirs
  where gs   = groupBy ((==) `on` fst) . reverse $ dtwPath
        gs'  = [ (fst . head $ g, map snd g) | g <- gs ]
        dirs = [ sum [ ((patch V.! y) - (φ V.! x) ) / genericLength ys | y <- ys] | (x,ys) <- gs' ]
        (Result _ dtwPath) = eventDTW φ patch



-- | draw 'n' sequential events from 'es', convert them to the internal
-- 'Event' type and normalize the timestamps
drawPatch ::
  (Fractional a, Ord a, G.Vector v (DVS.Event DVS.Address),
   MonadRandom f) =>
  Int -> v (DVS.Event DVS.Address) -> f (V.Vector (Event a))
drawPatch n es = normalizeEventTimes . V.map fromDVSEvent . V.convert <$> pickRandomEvents n es 

{-test = do-}
{-    let patchSize = 64-}
{-    let patchNum  = 8-}


{-    return ()-}
