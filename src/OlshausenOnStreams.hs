
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
import           Control.Lens

import           Data.Thyme.Clock
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import           Data.Function
import           Data.List
import           Data.Word7
import           Data.Bifunctor

import           Data.DTW

import           Data.List.Ordered

import qualified Numeric.AD as AD

import           Linear


import           OlshausenOnStreams.ArtificialData
import qualified OlshausenOnStreams.AERPlot as Plot


type Event a  = V4 a
type Events a = V.Vector (Event a)
type Patch a  = Events a
type Phi a    = Events a

time, posX, posY, pol :: Lens' (Event a) a
time = _x
posX = _y
posY = _z
pol  = _w

fromDVSEvent :: Fractional a => DVS.Event DVS.Address -> Event a
fromDVSEvent (DVS.Event (DVS.Address p x y) t) = 
    V4 (toSeconds t) (fromIntegral x) (fromIntegral y) (if p == DVS.U then 1 else -1)

createRandomEvent :: (Num a, Random a, MonadRandom m) => m (Event a)
createRandomEvent = V4 <$> getRandomR (0,1) <*> getRandomR (0,127) <*> getRandomR (0,127) <*> getRandomR (0,1)


-- | create random eventstream of length n
createRandomEvents :: (U.Unbox a, Ord a, Num a, Random a, MonadRandom m) => Int -> m (Events a)
createRandomEvents n = sortEvents <$> G.replicateM n createRandomEvent




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

{-scaleSTC :: (Monad m, Num a) => a -> a -> a -> a -> m (Event a) -> m (Event a)-}
{-scaleSTC ft fx fy fp stc = [ Event (ft * t) (fx * x) (fy * y) (fp * p) | (Event t x y p) <- stc ]-}


-- | TODO fix value NaNs ... hacked for now
normalizeSTC es = fmap (set pol 1) [ (e - m) / s | e <- es ]
  where m = mean es
        s = stdDev es

{-onNormalizedSTC :: (Floating a, Monad t, Foldable t) => t a -> (t a -> (b,t a)) -> (b,t a)-}
onNormalizedSTC stc f = unnormalize . f . normalize $ stc
  where normalize es = [ (e - m) / s | e <- es ]
        unnormalize es = [ (e * s) + m | e <- es ]
        m = mean stc
        s = stdDev stc


reconstructEvents :: (Ord a, Num a) => [a] -> [Events a] -> Events a
{-reconstructEvents as φs = mergeEvents $ V.zipWith (\a φ -> V.map (a *^) φ) as φs-}
reconstructEvents as φs = mergeEvents $ [ [ a *^ e | e <- φ ] | a <- as | φ <- φs ]


-- | sort events based on timestamp
sortEvents :: (Ord a) => Events a -> Events a
sortEvents = G.modify (V.sortBy (compare `on` (^. time)))

{-concatEvents :: Ord a => [Events a] -> Events a-}
{-concatEvents es = sortEvents $ V.concat es-}

-- | merge multiple sorted event streams
mergeEvents :: Ord a => [Events a] -> Events a
mergeEvents = G.fromList . mergeAllBy (compare `on` (view time)) . map G.toList . sortHeads
    where sortHeads = sortBy (compare `on` (view time . G.head))

-- | this is basically euclidian distance in space-time
eventDistance :: Floating a => Event a -> Event a -> a
eventDistance a b = sqrt (positionDistance + timeDistance + valDistance)
  where positionDistance = (a ^. posX - b ^. posX)**2 + (a ^. posY - b ^. posY)**2
        timeDistance     = (a ^. time - b ^. time)**2
        valDistance      = (a ^. pol  - b ^. pol )**2



mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / (fromIntegral $ length xs)

stdDev :: (Floating a, Monad t, Foldable t) => t a -> a
stdDev xs = sqrt ((1 / (fromIntegral $ length xs)) * sum [ (x - m)^^2 | x <- xs ])
  where m = mean xs

-- | super mega generic type 0o
{-eventDTW :: (Floating a, Ord a, G.Vector v e, e ~ Event a, Item (v e) ~ Event a, DataSet (v e)) -}
{-         => v e -> v e -> Result a-}
eventDTW :: (Ord a, Floating a) => Events a -> Events a -> Result a
eventDTW = fastDtw eventDistance reduceEventStream 1

-- | cut the eventstream in half by skipping every second event
reduceEventStream :: Events a -> Events a
reduceEventStream v = G.backpermute v (G.fromList [0,2..G.length v - 1])


sparseness :: Floating a => a -> a -> a
sparseness σ a = s (a / σ) where s x = log (1+x*x)

logit x = log (1+x*x)

-- | TODO give parameters as ... parameters
errorFunction :: (Floating a, Ord a)
  => Events a -> [Events a] -> [a] -> a
errorFunction patch φs as = λ   * cost (eventDTW patch (reconstructEvents as φs))
                          + bos * sum [ sparseness σ a | a <- as ]
    where bos = 6.96
          λ   = 100
          σ   = 0.316 


doTheTimeWarp :: forall a. (Ord a, Num a, Floating a)
              => V.Vector (Event a) -> [V.Vector (Event a)] -> [a] -> [[a]]
doTheTimeWarp patch φs = AD.gradientDescent go
  where go :: forall t. (AD.Scalar t ~ a, AD.Mode t, Floating t, Ord t) => [t] -> t
        go = errorFunction (fmap AD.auto <$> patch) (fmap (fmap AD.auto) <$> φs)

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
  V.Vector (Event t1) -> V.Vector (Event t1) -> V.Vector (Event t1)
getPushDirections φ patch = V.fromList dirs
  where gs   = groupBy ((==) `on` fst) . reverse $ dtwPath
        gs'  = [ (fst . head $ g, map snd g) | g <- gs ]
        dirs = [ sum [ ((patch V.! y) - (φ V.! x) ) / genericLength ys | y <- ys] | (x,ys) <- gs' ]
        (Result _ dtwPath) = eventDTW φ patch


updatePhi :: (Floating a, Ord a) => a -> Events a -> Events a -> Events a
updatePhi η φ patch = V.zipWith (+) φ (V.map ((η*c) *^) pds)
  where pds = getPushDirections φ patch
        c   = min 1 (1 / (cost $ eventDTW φ patch))



oneIteration :: (Floating a, Ord a) => [Events a] -> [Events a] -> [Events a]
oneIteration patches φs = fmap (\φ -> foldl' normUpdate φ patches) φs
  where η           = 1 / (fromIntegral . length $ patches)
        normPatches = map normalizeSTC patches
        normUpdate φ patch = onNormalizedSTC φ (\φ -> updatePhi η φ (normalizeSTC patch))





{-test = do-}
{-    let patchSize = 64-}
{-    let patchNum  = 8-}


{-    return ()-}
