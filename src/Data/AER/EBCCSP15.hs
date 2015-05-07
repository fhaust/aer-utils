
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Data.AER.EBCCSP15 where


import           Data.Foldable

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector          as B

import           Data.List
import           Data.List.Split

import           Data.Proxy

import           Numeric.LinearAlgebra.HMatrix
import           Numeric.AD

import           Control.Monad.Random
import           Control.Monad hiding (forM_,mapM,mapM_)

import           Codec.Picture

import           System.Directory

import           Control.Parallel.Strategies

import           GHC.TypeLits


{-import           Debug.Trace-}




infixl 4 <$$>
(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f0 (f1 a) -> f0 (f1 b)
f <$$> x = fmap (fmap f) x


-----------------------------------------------------------------------------

newtype Mat (w :: Nat) (h :: Nat) a = Mat { unMat :: B.Vector a } deriving (Functor,Show,Read)

type Img w h a = Mat w h a
type Phi w h a = Mat w h a

instance Num a => Num (Mat (w :: Nat) (h :: Nat) a) where
    (+) (Mat a) (Mat b) = Mat $ V.zipWith (+) a b
    (-) (Mat a) (Mat b) = Mat $ V.zipWith (-) a b
    (*) (Mat a) (Mat b) = Mat $ V.zipWith (*) a b
    negate (Mat a)      = Mat $ V.map negate a
    abs    (Mat a)      = Mat $ V.map abs a
    signum (Mat a)      = Mat $ V.map signum a
    fromInteger a       = Mat $ V.replicate 64 (fromInteger a)
    {-# INLINABLE (+) #-}
    {-# INLINABLE (-) #-}
    {-# INLINABLE (*) #-}
    {-# INLINABLE negate #-}
    {-# INLINABLE abs #-}
    {-# INLINABLE signum #-}

scale' :: Num a => a -> Mat w h a -> Mat w h a 
scale' f = Mat . V.map (*f) . unMat
{-# INLINABLE scale' #-}

add' :: Num a => Mat w h a -> Mat w h a -> Mat w h a
add' a b = Mat $ V.zipWith (+) (unMat a) (unMat b)
sub' :: Num a => Mat w h a -> Mat w h a -> Mat w h a
sub' a b = Mat $ V.zipWith (-) (unMat a) (unMat b)
mul' :: Num a => Mat w h a -> Mat w h a -> Mat w h a
mul' a b = Mat $ V.zipWith (*) (unMat a) (unMat b)
pow' :: (Integral b, Num a) => Mat w h a -> b -> Mat w h a
pow' a e = Mat $ V.map (^e) (unMat a)

norm' (Mat a) = sqrt . V.sum $ V.zipWith (*) a a

variance' :: Fractional a => Mat w h a -> a
variance' (Mat a) = vecVariance a

vecVariance a = V.sum (V.map (\x -> (x - mean)^(2::Int)) a) / len
  where mean = vecMean a
        len  = fromIntegral $ V.length a

vecMean a = V.sum a / fromIntegral (V.length a)

sumElems' :: Num a => Mat w h a -> a
sumElems' = V.sum . unMat

zipWith' :: (a -> b -> c) -> Mat w h a -> Mat w h b -> Mat w h c
zipWith' f (Mat a) (Mat b) = Mat $ V.zipWith f a b

fromList' :: [a] -> Mat w h a
fromList' l = Mat $ V.fromList l

{-subMat' :: Int -> Int -> Int -> Int -> Mat a -> Mat a-}
{-subMat' ox oy w h = -}

--------------------------------------------------

reconstruct :: Num a => [a] -> [Phi w h a] -> Img w h a
reconstruct as φs = sum [ a `scale'` φ | a <- as | φ <- φs ]

sparseness :: Floating a => a -> a -> a
sparseness σ a = s (a / σ) where s x = log (1+x*x)

preserveInfos :: Num a => Img w h a -> [Phi w h a] -> [a] -> a
preserveInfos img φs as = sumElems' $ (img - reconstruct as φs) ^ (2::Int)

errorFunction ::
  Floating t => t -> t -> Mat w h t -> [Mat w h t] -> [t] -> t
errorFunction λ σ img φs as = preserveInfos img φs as + λ * sum [ sparseness σ a | a <- as ]

findAsForImg :: forall w h a. (Floating a, Ord a)
             => a -> a -> Img w h a -> [Phi w h a] -> [a] -> [[a]]
findAsForImg λ σ img φs = conjugateGradientDescent go
  where go :: forall t. (Scalar t ~ a, Mode t, Floating t) => [t] -> t
        go = errorFunction (auto λ) (auto σ) (auto <$> img) (auto <$$> φs)

initialAs :: Fractional a => Img w h a -> [Phi w h a] -> [a]
initialAs img = map go
  where go φ = sumElems' (φ * img) / norm
          where norm = sumElems' $ φ^(2::Int)

residualError :: Num a => [Img w h a] -> [[a]] -> [Phi w h a] -> [Img w h a]
residualError imgs ass φs = [ img - reconstruct as φs | img <- imgs | as <- ass ]

updateBases ::
  (Fractional a, Num a) => [Mat w h a] -> [[a]] -> [Mat w h a]
updateBases es ass = map (le `scale'`) dA
    where dA = foldl1' (zipWith (+)) [[ a `scale'` e | a <- as ] | e <- es | as <- ass ]
          le = 1 / genericLength es

-- this code corresponds to line 71-78 in the sparsenet code

varΗ, varGoal :: Double
varΗ = 0.001
varGoal = 0.1

adjustAVar :: [Double] -> [Double] -> [Double]
adjustAVar vars as = zipWith (+) [ var * (1-varΗ) | var <- vars ] [ varΗ * a^(2::Int) | a <- as ]

adjustAVars :: [Double] -> [[Double]] -> [Double]
adjustAVars = foldl' adjustAVar

adjustGain :: Double -> [Double] -> [Double] -> [Double]
adjustGain α vars gain = zipWith (*) gain (map ((**α) . (/varGoal)) vars)


adjustPhiVariance :: Fractional a => [a] -> [a] -> [Phi w h a] -> [Phi w h a]
adjustPhiVariance gains norms φs = zipWith3 (\g n φ -> (g/n) `scale'` φ) gains norms φs

adjustPhisForVariance α vars gain ass φs = 
    let vars' = adjustAVars vars ass
        normA = map norm' φs
        gain' = adjustGain α vars' gain
    in adjustPhiVariance gain' normA φs

----


--oneIteration ::
--  (Floating a, Ord a) => a -> a -> a -> [Img w h a] -> [Phi w h a] -> [Phi w h a]
--oneIteration λ σ η imgs φs =
--    let ass  = [ initialAs img φs | img <- imgs ]
--        ass' = [ findAsForImg λ σ img φs as !! 10 | img <- imgs | as <- ass ]
--        es   = residualError imgs ass' φs
--        dφs  = updateBases es ass'
--    in [ φ - η `scale'` dφ | φ <- φs | dφ <- dφs ]

--oneIteration imgs = do

--    -- choose an image for this batch
--    img <- uniform imgs

--    -- extract subimages at random from this image to make data vector X


main :: IO ()
main = do

    let numPatches = 100

    imgs <- readCSVMat "data/mats/x.csv" :: IO [Img 512 512 Double]
    phis <- readCSVMat "data/mats/a.csv" :: IO [Phi 64 64 Double]

    let λ = 100
        σ = 0.316
        η = 1
    --let phis' = oneIteration λ σ η imgs phis

    --print phis'


    putStrLn "done"


--------------------------------------------------

--readAsMat :: FilePath -> IO (Matrix Float)
--readAsMat fn = do
--    (Right (ImageY16 img)) <- readImage fn
--    let f = pixelMap ((/(2^^(16::Int)-1)) . fromIntegral) img :: Image Float
--    return $ img2mat f

--randomPatch :: Int -> Matrix Float -> IO (Matrix Float)
--randomPatch s m = do
--    let t = s `div` 2
--    c <- randomRIO (t, cols m - t)
--    r <- randomRIO (t, rows m - t)

--    return $ subMatrix (r-t,c-t) (s,s) m

--createRandomPatchesFromImage :: FilePath -> IO () 
--createRandomPatchesFromImage imagePath = do
--    -- read in test image
--    putStrLn "reading image"
--    (Right (ImageY16 raw)) <- readPng imagePath

--    -- convert in matrix
--    let img :: Image Float
--        img = pixelMap ( (/(2^^(16::Int)-1)) . fromIntegral ) raw
--        mat = img2mat img

--    -- draw random submatrices
--    ms <- replicateM 512 (randomPatch 16 mat)
--    let ps = map mat2img ms :: [Image PixelF]


--    -- write out patches as images
--    forM_ (zip [0..] ps) $ \(i,p) -> do
--      let path = "/tmp/imgs/img" ++ show i ++ ".png"
--          img  = pixelMap (round . (*(2^(16::Int)))) p :: Image Pixel16
--      putStrLn $ "writing: " ++ path
--      putStrLn $ "maximum: " ++ show (V.maximum . imageData $ img)
--      putStrLn $ "minimum: " ++ show (V.minimum . imageData $ img)
--      writePng path img

--------------------------------------------------


mat2img :: Element (PixelBaseComponent a) 
        => Matrix (PixelBaseComponent a) -> Image a
mat2img m = Image (cols m) (rows m) (flatten m)
img2mat :: S.Storable (PixelBaseComponent a) 
        => Image a -> Matrix (PixelBaseComponent a)
img2mat i = reshape (imageWidth i) $ imageData i


mat2mat' :: Element a => Matrix a -> Mat w h a
mat2mat' m = Mat . V.convert $ flatten m

mat'2img ::
  S.Storable (PixelBaseComponent a) =>
  Int -> Int -> Mat w h (PixelBaseComponent a) -> Image a
mat'2img w h m = Image w h (V.convert . unMat $ m)


readCSVMat :: forall w h a. (KnownNat w, Read a) => FilePath -> IO [Mat w h a]
readCSVMat fp = do
    f <- readFile fp
    let ds =  map read . filter (/= "") . splitWhen (\x -> elem x ("\n," :: String)) $ f
        w  = fromInteger $ natVal (Proxy :: Proxy w)
    -- make sure that we get no half matrices
    when (length ds `mod` w /= 0) $ error "readCSVMat: dimensions do not match"
    return $ map (Mat . B.fromList) $ chunksOf w ds

readCSVVec :: (UV.Unbox a, Read a) => Int -> FilePath -> IO [UV.Vector a]
readCSVVec n fp = do
    f <- readFile fp
    let ds =  map read . filter (/= "") . splitWhen (\x -> elem x ("\n," :: String)) $ f
    return $ map UV.fromList $ chunksOf n ds

readCSVImage :: (UV.Unbox a, Read a) => FilePath -> IO (Mat w h a)
readCSVImage fp = do
    f <- readFile fp
    let w  = fromInteger $ natVal (Proxy :: Proxy w)
        h  = fromInteger $ natVal (Proxy :: Proxy h)
        ds = map read . filter (/= "") . splitWhen (\x -> elem x ("\n," :: String)) $ f
    return . Mat . V.fromListN (w*h) $ ds


{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c


iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = f a >>= iterateM f
