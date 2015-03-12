

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where



import Data.AER.Tmpdiff128StereoPair
import Data.AER.Types

import Data.Thyme.Clock
import Data.Maybe
import Data.List
import Data.Function

import Control.Applicative
import Control.Monad.Random
import Control.Monad

import Math.KMeans

import           Numeric.LinearAlgebra.HMatrix

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Generic as G

import Debug.Trace

dview :: Event Address -> (Bool,Int,Int,Bool,Float)
dview (qview -> (p,x,y,c,t)) = ( p == U
                              , fromIntegral x
                              , fromIntegral y
                              , c == L
                              , toSeconds t
                              )

isLeftEvent (qview -> (_,_,_,c,_)) = c == L
isRightEvent (qview -> (_,_,_,c,_)) = c == R
isUpEvent (qview -> (p,_,_,_,_)) = p == U
isDownEvent (qview -> (p,_,_,_,_)) = p == D

infixr 4 <$$>
f <$$> x = fmap (fmap f) x

type Orientation = (Float,Float,Float,NominalDiffTime)

main = do


    -- read in the data
    (Right es) <- V.fromList <$$> readStereoData "../aer/data/Tmpdiff128StereoPair-vertical-arm.aedat"

    -- unzip events into four streams (up/down, left/right)
    let leftUpEs    = V.filter (\e -> isLeftEvent e  && isUpEvent e) es
        leftDownEs  = V.filter (\e -> isLeftEvent e  && isDownEvent e) es
        rightUpEs   = V.filter (\e -> isRightEvent e && isUpEvent e) es
        rightDownEs = V.filter (\e -> isRightEvent e && isDownEvent e) es


    -- accumulate the timestamps
    let zeroTs      = UV.replicate (128*128) 0
        leftUpTimes = V.scanl' (\ts e -> updateTimeVec e ts) zeroTs leftUpEs
        rightUpTimes = V.scanl' (\ts e -> updateTimeVec e ts) zeroTs rightUpEs

    -- extract plane normals
    let leftUpPlanes = V.zipWith (extractPlane 4) leftUpEs leftUpTimes
        rightUpPlanes = V.zipWith (extractPlane 4) rightUpEs rightUpTimes

    -- do kmeans clustering on the plane normals
    let numMeans = 5
    leftUpMeans  <- kmeansWith randomPartition id euclidSq numMeans (map V.convert . catMaybes $ V.toList leftUpPlanes)
    {-rightUpMeans <- kmeans (V.convert.snd) euclidSq numMeans (catMaybes $ V.toList rightUpPlanes)-}

    G.mapM_ print leftUpMeans
    {-G.mapM_ print rightUpMeans-}
    {-print rightUpMeans-}

    {-let leftUpStuff  = V.zipWith (\a b -> (a,) <$> b) leftUpEs leftUpPlanes-}
    {-    rightUpStuff  = V.zipWith (\a b -> (a,) <$> b) rightUpEs rightUpPlanes-}

    {-    stuff = sortBy (compare `on` (timestamp.fst))-}
    {-          . catMaybes $ V.toList (leftUpStuff V.++ rightUpStuff)-}

    {-writeFile "/tmp/stuff.txt" ""-}
    {-mapM_ (appendFile "/tmp/stuff.txt" . ('\n':) . show) stuff-}

    putStrLn "stuff"

type Mat a = UV.Vector a

matIdx x y = x + y * 128
vecIdx i = i `quotRem` 128

writeMat x y a = UV.modify (\v -> UMV.write v (matIdx x y) a)
readMat x y v = v UV.! (matIdx x y)

updateTimeVec :: Event Address -> Mat Float -> Mat Float
updateTimeVec (dview -> (_,x,y,_,t)) v = writeMat (fromIntegral x) (fromIntegral y) t v


extractPlaneCands :: Int -> Int -> Int -> Mat Float -> [(Int,Int,Float)]
extractPlaneCands r x y m = [ (x',y', readMat x' y' m) | y' <- [y-r..y+r]
                                                       , x' <- [x-r..x+r]
                                                       , x' >= 0 && x' < 128
                                                       , y' >= 0 && y' < 128
                                                       ]

extractPlane :: Int -> Event Address -> Mat Float -> Maybe (Vector Double)
extractPlane r (dview -> (_,x,y,_,t)) m = test $ (toNormalForm <$> leastSquares cands)
    where cands = filter notTooOld $ extractPlaneCands r x y m
          notTooOld (_,_,ct) = abs(ct - t) < 0.01
          numCands = length cands
          test = if numCands < 4 then const Nothing else id

toNormalForm :: Vector Double -> Vector Double
toNormalForm v = correctSign $ vector [a/n, b/n, c/n]
  where n = sqrt (a*a + b*b + c*c)
        [a,b,c] = G.toList v
        correctSign = if (c/n) < 0 then scale (-1) else id

-- http://stackoverflow.com/questions/1400213/3d-least-squares-plane
leastSquares :: [(Int,Int,Float)] -> Maybe (Vector Double)
leastSquares cs = test $ app (inv m) v
    where (m,v) = leastSquaresMatVec cs
          test  = if det m /= 0 then Just else const Nothing

leastSquaresMatVec cs = (m,v)
    where m00 = sum $ zipWith (*) xis xis
          m10 = sum $ zipWith (*) xis yis
          m20 = sum xis
          m01 = sum $ zipWith (*) xis yis
          m11 = sum $ zipWith (*) yis yis
          m21 = sum yis
          m02 = sum xis
          m12 = sum yis
          m22 = fromIntegral $ length cs
          m   = matrix 3 [m00,m10,m20,m01,m11,m21,m02,m12,m22]
          

          v0  = sum $ zipWith (*) xis zis
          v1  = sum $ zipWith (*) yis zis
          v2  = sum zis
          v   = vector [v0,v1,v2]

          xis = map (fromIntegral.fst) cs
          yis = map (fromIntegral.snd) cs
          zis = map (realToFrac . thd) cs
          fst (a,_,_) = a
          snd (_,b,_) = b
          thd (_,_,c) = c

-- sum_i x[i]*x[i],    sum_i x[i]*y[i],    sum_i x[i]
-- sum_i x[i]*y[i],    sum_i y[i]*y[i],    sum_i y[i]
-- sum_i x[i],         sum_i y[i],         n
--
-- {sum_i x[i]*z[i],   sum_i y[i]*z[i],    sum_i z[i]}



randomPartition :: MonadRandom m => Int -> [UV.Vector Double] -> m (Clusters (UV.Vector Double))
randomPartition k xs = do
    -- draw k random positions from xs
    ms <- replicateM k (uniform xs)


    -- calculate cluster distances
    let ds = map (Cluster . map fst)
           . groupBy ((==) `on` snd)
           . sortBy (compare `on` snd)
           . map (\x -> minimumBy (compare `on` (uncurry euclidSq)) [ (x,m) | m <- ms ]) 
           $ xs

    return $ V.fromList ds
    

