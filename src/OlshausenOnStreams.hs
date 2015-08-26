
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OlshausenOnStreams  where


import qualified Data.AER.DVS128 as DVS


import           Control.Monad.Random
import           Control.Monad
import           Control.Parallel.Strategies
import           Control.Lens

import           Data.Thyme.Clock
import           Data.Thyme.Time
import           Data.Thyme.Format
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import           Data.Function
import           Data.List
import           Data.Word7
import           Data.Foldable
import           Data.Binary
import           Data.AffineSpace

import           Data.DTW

import           Data.List.Ordered

import qualified Numeric.AD as AD

import           Linear

import           System.Locale
import           System.Directory

import           OlshausenOnStreams.ArtificialData
import           OlshausenOnStreams.Plotting

import           Debug.Trace


type Event a   = V4 a
type Events a  = V.Vector (Event a)
type Patch a   = Events a
type Patches a = [Events a]
type Phi a     = Events a
type Phis a    = [Events a]

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

extractRandomSTC ::
  MonadRandom m =>
  NominalDiffTime
  -> Word7
  -> Word7
  -> [DVS.Event DVS.Address]
  -> m [DVS.Event DVS.Address]
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
normalizeSTC ::
  (Floating a, Monad f, Foldable f) => f (Event a) -> f (Event a)
normalizeSTC es = fmap (set pol 1) [ (e - m) / s | e <- es ]
  where m = mean es
        s = stdDev es

onNormalizedSTC ::
  (Floating b, Monad m, Monad m1, Foldable m1) =>
  m1 b -> (m1 b -> m b) -> m b
onNormalizedSTC stc f = unnormalize . f . normalize' $ stc
  where normalize' es = [ (e - m) / s | e <- es ]
        unnormalize es = [ (e * s) + m | e <- es ]
        m = mean stc
        s = stdDev stc

reconstructEvents :: (Ord a, Num a) => [Event a] -> [Events a] -> Events a
reconstructEvents as φs = mergeEvents $ zipWith (\a φ -> V.map (\e -> a * e) φ) as φs
{-reconstructEvents as φs = mergeEvents $ [ [ a * e | e <- φ ] | a <- as | φ <- φs ]-}


-- | sort events based on timestamp
sortEvents :: (Ord a) => Events a -> Events a
sortEvents = G.modify (V.sortBy (compare `on` (^. time)))

{-concatEvents :: Ord a => [Events a] -> Events a-}
{-concatEvents es = sortEvents $ V.concat es-}

-- | merge multiple sorted event streams
mergeEvents :: Ord a => [Events a] -> Events a
mergeEvents = G.fromList . mergeAllBy (compare `on` (view time)) . map G.toList . sortHeads
    where sortHeads = sortBy (compare `on` (view time . G.head))



mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / (fromIntegral $ length xs)

stdDev :: (Floating a, Monad t, Foldable t) => t a -> a
stdDev xs = sqrt ((1 / (fromIntegral $ length xs)) * sum [ (x - m)^^(2::Int) | x <- xs ])
  where m = mean xs

-- | super mega generic type 0o
{-eventDTW :: (Floating a, Ord a, G.Vector v e, e ~ Event a, Item (v e) ~ Event a, DataSet (v e)) -}
{-         => v e -> v e -> Result a-}
eventDTW :: (Ord a, Floating a) => Events a -> Events a -> Result a
eventDTW = fastDtw qd reduceEventStream 1

-- type REvent s = Event (AD.Reverse s Float)
-- type REvents s = Events (AD.Reverse s Float)
-- 
-- {-# SPECIALIZE eventDTW :: forall s. Reifies s AD.Tape => Events (AD.Reverse s Float) -> Events (AD.Reverse s Float) -> Result (AD.Reverse s Float) #-}
-- 


-- | cut the eventstream in half by skipping every second event
reduceEventStream :: Events a -> Events a
reduceEventStream v = G.backpermute v (G.fromList [0,2..G.length v - 1])

sparseness :: Floating a => a -> a -> a
sparseness σ a = s (a / σ) where s x = log (1+x*x)

logit :: Floating a => a -> a
logit x = log (1+x*x)

-- | TODO give parameters as ... parameters
errorFunction :: (Floating a, Ord a)
  => Events a -> [Events a] -> [Event a] -> a
errorFunction patch φs as = r -- + b
    where bos = 6.96
          λ   = 100
          σ   = 0.316 
          r   = λ   * cost (eventDTW patch (reconstructEvents as φs))
          b   = bos * sum [ sum (sparseness σ <$> a) | a <- as ]


doTheTimeWarp :: forall a. (Ord a, Num a, Floating a)
              => Events a -> [Events a] -> [Event a] -> [[Event a]]
doTheTimeWarp patch φs as = fromFlatList <$> AD.gradientDescent go (toFlatList as)
  where go :: forall t. (AD.Scalar t ~ a, AD.Mode t, Floating t, Ord t) => V.Vector t -> t
        go ts = errorFunction (AD.auto <$$> patch) (AD.auto <$$$> φs) (fromFlatList ts)



toFlatList :: [Event a] -> V.Vector a
toFlatList = V.concat . fmap (V.fromList . toList)

fromFlatList :: V.Vector a -> [Event a]
fromFlatList v | V.null v  = []
               | otherwise = toV h : fromFlatList t
    where (h,t) = V.splitAt 4 v
          toV (toList -> [a,b,c,d]) = V4 a b c d
          toV _ = error "shouldn't happen"
{-fromFlatList []           = []-}
{-fromFlatList (a:b:c:d:xs) = (V4 a b c d) : fromFlatList xs-}
{-fromFlatList _            = error "shouldn't be used on lists that aren't of quadruples"-}


doTheTimeWarpAgain :: (Floating a, Ord a) 
                   => Events a -> [Events a] -> [Event a] -> [Event a]
doTheTimeWarpAgain patch φs = go . doTheTimeWarp patch φs
  where go (a:b:xs) | errDiff a b < 0.01 = b
                    | otherwise          = go (b:xs)
        go _        = error "shouldn't happen"
        errFun = errorFunction patch φs 
        errDiff a b = abs (errFun a - errFun b)

{--- | calculate as per patch, outer list is per patch, inner is as -}
{-fitAs :: MonadRandom m-}
{-      => [Patch Float] -> [Phi Float] -> m [[Event Float]]-}
{-fitAs patches φs = mapM (\patch -> fitSingleAs patch φs) patches-}

{-fitSingleAs ::-}
{-  (Floating a, Ord a, Random a, MonadRandom m) =>-}
{-  Events a -> [Events a] -> [a] -> m [Event a]-}
{-fitSingleAs patch φs as = do-}


{-    -- do gradient descent to find "perfect" as for this patch-}
{-    return $ doTheTimeWarpAgain patch φs as-}



-- | calculate the "vector" along which the phi has to be pushed to get
-- closer to the patch
getPushDirections ::
  (Floating t1, Ord t1) =>
  Phi t1 -> Patch t1 -> V.Vector (Event t1)
getPushDirections φ patch = V.fromList dirs
  where gs   = groupBy ((==) `on` fst) . reverse $ dtwPath
        gs'  = [ (fst . head $ g, map snd g) | g <- gs ]
        dirs = [ sum [ ((patch V.! y) - (φ V.! x) ) / genericLength ys | y <- ys] | (x,ys) <- gs' ]
        (Result _ dtwPath) = eventDTW φ patch


updatePhi :: (Floating a, Ord a) => a -> Events a -> Events a -> Events a
updatePhi η φ patch = V.zipWith (+) φ (V.map ((η*c) *^) pds)
  where pds = getPushDirections φ patch
        c   = min 1 (1 / (cost $ eventDTW φ patch))



oneIteration' ::
  (Show a, Floating a, Ord a, Random a, MonadRandom m, NFData a) =>
  [Patch a] -> [Phi a] -> m [Phi a]
oneIteration' patches φs = do

    let numPatches = length patches

    -- generate some random as
    randomAs <- replicateM (length patches) $ replicateM (length φs) getRandom

    -- calculate push directions for all patches
    let scaledDirections = foldl1' (zipWith (V.zipWith (\a b -> a + b ^/ (fromIntegral numPatches))))
                         $ withStrategy (parList rdeepseq) 
                         $ zipWith (oneIterationPatch φs) patches randomAs

    {-traceM $ "finalDirections: " ++ show scaledDirections-}

    -- apply updates
    let updatedφs = zipWith (V.zipWith (+)) φs scaledDirections

    {-traceM $ "updatedφs: " ++ show updatedφs-}

    -- normalize φs again -> done
    {-return $ map normalizeSTC updatedφs-}
    return updatedφs

oneIterationPatch :: (Ord a, Floating a) => [Phi a] -> Patch a -> [V4 a] -> [V.Vector (V4 a)]
oneIterationPatch φs patch randomAs = scaledDirections 

          -- find as that best represent the given patch
    where fittedAs = doTheTimeWarpAgain patch φs randomAs

          -- scale phis with the given as
          fittedφs = zipWith (\a φ -> (a*) <$> φ) fittedAs φs
          {-fittedφs = normalizedφs-}

          -- at this point the phis are normalized and scaled according to
          -- the 'best' as, that is they match the normalized patches as
          -- best as possible while just scaling in t,x,y and p.

          pushDirections = map (\φ -> getPushDirections φ patch) fittedφs


          -- push directions are scaled with the kai factor:
          kaiFactor      = map (\φ -> min 1 ( 1 / (cost $ eventDTW φ patch))) fittedφs

          scaledDirections = zipWith (\f d -> V.map (f *^) d) kaiFactor pushDirections


instance Random a => Random (V4 a) where
    randomR (V4 lx ly lz lw, V4 hx hy hz hw) 
      = runRand (V4 <$> getRandomR (lx,hx) <*> getRandomR (ly,hy) <*> getRandomR (lz,hz) <*> getRandomR (lw,hw))
    random = runRand (V4 <$> getRandom <*> getRandom <*> getRandom <*> getRandom)


iterateNM :: Int -> (a -> IO a) -> a -> IO [a]
iterateNM 0 _ _ = return []
iterateNM n f x = do
    tStart <- getCurrentTime
    x' <- f x 
    tEnd <- getCurrentTime
    traceM $ "iteration " ++ show n ++ " took " ++ show (toMicroseconds (tEnd .-. tStart)) ++ "µs"
    xs' <- iterateNM (n-1) f x'
    return $ x' : xs'

test :: IO ()
test = do

    traceM "running"
    
    t <- formatTime defaultTimeLocale "%F_%T" <$> getCurrentTime
    let dn = "data/test_" ++ t ++ "/" 
    createDirectoryIfMissing True dn

    let numPhis = 8
        sizePhis = 16
        iterations = 500

    -- create random patches
    {-stcs <- replicateM 2 (sortEvents . V.fromList <$> randomPlane 128) :: IO [Events Float]-}
    a <- normalizeSTC . sortEvents . V.fromList <$> plane (V3 0 0 0) (V3 0 1 1) 128 :: IO (Events Float)
    b <- normalizeSTC . sortEvents . V.fromList <$> plane (V3 0 0 0) (V3 0 1 (-1)) 128 :: IO (Events Float)
    let stcs = [a,b]
    encodeFile (dn ++ "stcs.bin") (toList <$> stcs)

    -- generate initial random phis
    phis <- map normalizeSTC <$> (replicateM numPhis $ createRandomEvents sizePhis) :: IO [Events Float]

    -- update φs many times
    phis' <- iterateNM iterations (oneIteration' stcs) phis :: IO [[Events Float]]


    encodeFile (dn ++ "phis.bin") (toList <$$> phis')

    -- show phis
    _ <- multiplotEvents . map normalizeSTC $ [a,b] ++ last phis'

    -- write images
    traceM "writing images"
    forM_ (zip [0::Int ..] phis') $ \(i,p) -> do
      let fn = dn ++ "it-" ++ show i ++ ".png"
      multiPlotFile fn . map normalizeSTC $ stcs ++ p


    return ()


infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x

infixl 4 <$$$>
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
f <$$$> x = fmap (fmap (fmap f)) x
