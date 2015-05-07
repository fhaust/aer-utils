
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.AER.EBCCSP15 where


import           Data.Foldable

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector          as B

import           Data.List
import           Data.List.Split

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

newtype Mat a = Mat { unMat :: B.Vector a } deriving (Functor,Show,Read)

type Img a = Mat a
type Phi a = Mat a

instance Num a => Num (Mat a) where
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

scale' :: Num a => a -> Mat a -> Mat a 
scale' f = Mat . V.map (*f) . unMat
{-# INLINABLE scale' #-}

add' :: Num a => Mat a -> Mat a -> Mat a
add' a b = Mat $ V.zipWith (+) (unMat a) (unMat b)
sub' :: Num a => Mat a -> Mat a -> Mat a
sub' a b = Mat $ V.zipWith (-) (unMat a) (unMat b)
mul' :: Num a => Mat a -> Mat a -> Mat a
mul' a b = Mat $ V.zipWith (*) (unMat a) (unMat b)
pow' :: (Integral b, Num a) => Mat a -> b -> Mat a
pow' a e = Mat $ V.map (^e) (unMat a)

norm' (Mat a) = sqrt . V.sum $ V.zipWith (*) a a

variance' :: Fractional a => Mat a -> a
variance' (Mat a) = vecVariance a

vecVariance a = V.sum (V.map (\x -> (x - mean)^(2::Int)) a) / len
  where mean = vecMean a
        len  = fromIntegral $ V.length a

vecMean a = V.sum a / fromIntegral (V.length a)

sumElems' :: Num a => Mat a -> a
sumElems' = V.sum . unMat

zipWith' :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWith' f (Mat a) (Mat b) = Mat $ V.zipWith f a b

fromList' :: [a] -> Mat a
fromList' l = Mat $ V.fromList l

{-subMat' :: Int -> Int -> Int -> Int -> Mat a -> Mat a-}
{-subMat' ox oy w h = -}

--------------------------------------------------

reconstruct :: Num a => [a] -> [Phi a] -> Img a
reconstruct as φs = sum [ a `scale'` φ | a <- as | φ <- φs ]

sparseness :: Floating a => a -> a -> a
sparseness σ a = s (a / σ) where s x = log (1+x*x)

preserveInfos :: Num a => Img a -> [Phi a] -> [a] -> a
preserveInfos img φs as = sumElems' $ (img - reconstruct as φs) ^ (2::Int)

errorFunction ::
  Floating t => t -> t -> Mat t -> [Mat t] -> [t] -> t
errorFunction λ σ img φs as = preserveInfos img φs as + λ * sum [ sparseness σ a | a <- as ]

findAsForImg :: forall a. (Floating a, Ord a) => a -> a -> Img a -> [Phi a] -> [a] -> [[a]]
findAsForImg λ σ img φs = conjugateGradientDescent go
  where go :: forall t. (Scalar t ~ a, Mode t, Floating t) => [t] -> t
        go = errorFunction (auto λ) (auto σ) (auto <$> img) (auto <$$> φs)

initialAs :: Fractional a => Img a -> [Phi a] -> [a]
initialAs img = map go
  where go φ = sumElems' (φ * img) / norm
          where norm = sumElems' $ φ^(2::Int)

residualError :: Num a => [Img a] -> [[a]] -> [Phi a] -> [Img a]
residualError imgs ass φs = [ img - reconstruct as φs | img <- imgs | as <- ass ]

updateBases ::
  (Fractional a, Num a) => [Mat a] -> [[a]] -> [Mat a]
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


adjustPhiVariance :: Fractional a => [a] -> [a] -> [Phi a] -> [Phi a]
adjustPhiVariance gains norms φs = zipWith3 (\g n φ -> (g/n) `scale'` φ) gains norms φs

adjustPhisForVariance α vars gain ass φs = 
    let vars' = adjustAVars vars ass
        normA = map norm' φs
        gain' = adjustGain α vars' gain
    in adjustPhiVariance gain' normA φs

----


oneIteration ::
  (Floating a, Ord a) => a -> a -> a -> [Img a] -> [Phi a] -> [Phi a]
oneIteration λ σ η imgs φs =
    let ass  = [ initialAs img φs | img <- imgs ]
        ass' = [ findAsForImg λ σ img φs as !! 10 | img <- imgs | as <- ass ]
        es   = residualError imgs ass' φs
        dφs  = updateBases es ass'
    in [ φ - η `scale'` dφ | φ <- φs | dφ <- dφs ]

--oneIteration imgs = do

--    -- choose an image for this batch
--    img <- uniform imgs

--    -- extract subimages at random from this image to make data vector X






--------------------------------------------------
eq1' :: Num a => [a] -> [Mat a] -> Mat a
eq1' as φs = foldl1 add' $ zipWith scale' as φs

eq3' :: Num a => Mat a -> [Mat a] -> [a] -> a
eq3' img φs as = negate $ sumElems' (errImg `pow'` (2::Int))
  where errImg = img `sub'` eq1' as φs

eq4a :: Floating a => a -> [a] -> a
eq4a σ as = - sum [ eqs (a/σ) | a <- as ]

eq6 :: Num a => a -> Mat a -> [Mat a] -> [a] -> a -> Mat a
eq6 η img φs as a = (η * a) `scale'` (img `sub'` eq1' as φs)

eqs :: Floating a => a -> a
eqs x = log (1+x*x)

errFun :: Floating a => Mat a -> [Mat a] -> [a] -> a
errFun m φs as = - eq3' m φs as - 0.14 * eq4a 0.14 as

errFun' :: Floating a => [Mat a] -> [Mat a] -> [a] -> a
errFun' ms φs as = sum [errFun m φs as | m <- ms]

diffTest :: forall a. (Fractional a, Floating a, Ord a) => Mat a -> [Mat a] -> [a] -> [[a]]
diffTest m φs = gradientDescent go
    where go :: forall t. (Scalar t ~ a, Mode t, Floating t) => [t] -> t
          go = errFun (auto <$> m) (auto <$$> φs)

diffTest' :: forall a. (Fractional a, Floating a, Ord a) => [Mat a] -> [Mat a] -> [a] -> [[a]]
diffTest' ms φs = gradientDescent go
    where go :: forall t. (Scalar t ~ a, Mode t, Floating t) => [t] -> t
          go = errFun' (auto <$$> ms) (auto <$$> φs)


-- | take the output of diffTest and give back the first result where
-- the error functions change is below "err" 
diffTill :: (Floating a, Ord a) => a -> Mat a -> [Mat a] -> [[a]] -> (a,[a])
diffTill err m φs ass = head $ dropWhile ((>err).fst) [ (e,as) | e <- es | as <- ass ]
    where es = [ abs (errFun m φs as - errFun m φs bs) | as <- ass | bs <- tail ass ]

-----------------------------------------------------------------------------

-- equation one: add up base functions and factors

eq1H :: (Container m a, Num (m a)) => [a] -> [m a] -> m a
eq1H as φs = sum [ a `scale` φ | a <- as | φ <- φs ]

eq3H :: (Container m a, Num (m a)) => m a -> [a] -> [m a] -> a
eq3H img as φs = - sumElements (errImg * errImg)
    where errImg = img - (eq1H as φs)

eq4aH :: Floating a => a -> [a] -> a
eq4aH σ as = - sum [ eqs (a/σ) | a <- as ]

eqsH x = log (1+x*x)
eqsH x = (2*x) / (x*x+1)


eq5H λ σ img φs as φ a = b - c - s
    where b = sumElements (φ * img)
          c = sum [ aj * (sumElements (φ * φj)) | aj <- as | φj <- φs ]
          s = (λ/σ) * eqsH (a / σ)

{-eq5s λ σ img φs as = [ eq5 λ σ img φs as φ a | φ <- φs | a <- as ]-}

eq6H η img as φs a = η `scale` ( a `scale` (img * eq1H as φs))

{-adEq5H :: forall m a. (Floating a, Ord a)-}
{-      => Matrix a -> [Matrix a] -> [a] -> [[a]]-}
{-adEq5H m φs as0 = gradientDescent go as0-}
{-  where go :: forall t. (Scalar t ~ a, Mode t, Element t, Floating t) => [t] -> t-}
{-        go as = - eq3H (cmap auto m) as (fmap (cmap auto) φs)  - 0.14 * eq4aH 0.14 as-}


main :: IO ()
main = do

    let numPatches = 100

    {-let imgPath = "data/swiss0-white.png"-}

    {-putStrLn "create 16x16 matrices"-}
    {-phis <- map single <$> replicateM numPatches (randn 16 16)-}
  
    {-putStrLn "create values"-}
    {-as <- replicateM numPatches randomIO :: IO [Float]-}

    {-putStrLn "read image"-}
    {-img <- readAsMat imgPath-}

    {-putStrLn "draw random patches"-}
    {-patches <- replicateM 100 (randomPatch 16 img)-}


    {--- just testing:-}
    {--- convert to mat format-}
    {-let phis'    = map mat2mat' phis-}
    {-let patches' = concat . repeat . map mat2mat' $ patches-}


    {-let results = iterate (uncurry3 (adjustPhis 0.14 0.2)) (patches',as,phis')-}
    {-[>let (_,as'',phis'') = results !! 10<]-}
  
    {-forM_ (zip [0..] results) $ \(i,(_,as,φs)) -> do-}
    {-  putStrLn $ "iteration " ++ show i-}
    {-  let baseFn = "output/iter-" ++ show i-}
    {-  createDirectoryIfMissing True baseFn-}
    {-  writePhiImages baseFn as φs-}

    imgs <- readCSVMat "data/mats/x.csv" :: IO [Img Double]
    phis <- readCSVMat "data/mats/a.csv" :: IO [Phi Double]

    let λ = 100
        σ = 0.316
        η = 1
    let phis' = oneIteration λ σ η imgs phis

    print phis'


    putStrLn "done"


{-withRandomPatches ::-}
{-  Matrix Float -> Int -> ([Matrix Float] -> IO b) -> IO b-}
{-withRandomPatches img n f = do-}
{-    patches <- replicateM n (randomPatch 16 img)-}
{-    f patches-}




adjustAs :: (Floating a, Ord a) => a -> Mat a -> [Mat a] -> [a] -> [a]
adjustAs σ img φs as = snd $ diffTill σ img φs (diffTest img φs as)

adjustFrom100Patches :: (Floating a, Ord a) => a -> [Mat a] -> [Mat a] -> [a] -> ([a], [Mat a])
adjustFrom100Patches σ allPatches φs as = (as',rest)
    where (patches,rest) = splitAt 100 allPatches
          as'        = foldl (\xs i -> adjustAs σ i φs xs) as patches

adjustPhis ::
  (Floating a, Ord a) =>
  a -> a -> [Mat a] -> [a] -> [Mat a] -> ([Mat a], [a], [Mat a])
adjustPhis σ η allPatches as φs = (rs,as',φs')
    where (as',r:rs) = adjustFrom100Patches σ allPatches φs as
          φs'        = [ φ `add'` eq6 η r φs as' a | a <- as' | φ <- φs ] `using` parListChunk 8 rseq

writePhiImages baseFn as φs = do
    let imgs = [ (i,a,mat'2img 16 16 (a `scale'` φ) :: Image Float) | a <- as | φ <- φs | i <- [0..] ]

    forM_ imgs $ \(ix,a,im) -> do
      let fn = baseFn ++ "/img-" ++ show ix ++ "-a-" ++ show a ++ ".png"
          img' = pixelMap (floor . (*255)) im :: Image Pixel8
      writePng fn img'

{-learngabors σ η allPatches = scanl (\(-}

{-findGabors patches φs as = do-}
  -- present 100 patches and adjust as until 1% is reached
  



--------------------------------------------------

readAsMat :: FilePath -> IO (Matrix Float)
readAsMat fn = do
    (Right (ImageY16 img)) <- readImage fn
    let f = pixelMap ((/(2^^(16::Int)-1)) . fromIntegral) img :: Image Float
    return $ img2mat f

randomPatch :: Int -> Matrix Float -> IO (Matrix Float)
randomPatch s m = do
    let t = s `div` 2
    c <- randomRIO (t, cols m - t)
    r <- randomRIO (t, rows m - t)

    return $ subMatrix (r-t,c-t) (s,s) m

createRandomPatchesFromImage :: FilePath -> IO () 
createRandomPatchesFromImage imagePath = do
    -- read in test image
    putStrLn "reading image"
    (Right (ImageY16 raw)) <- readPng imagePath

    -- convert in matrix
    let img :: Image Float
        img = pixelMap ( (/(2^^(16::Int)-1)) . fromIntegral ) raw
        mat = img2mat img

    -- draw random submatrices
    ms <- replicateM 512 (randomPatch 16 mat)
    let ps = map mat2img ms :: [Image PixelF]


    -- write out patches as images
    forM_ (zip [0..] ps) $ \(i,p) -> do
      let path = "/tmp/imgs/img" ++ show i ++ ".png"
          img  = pixelMap (round . (*(2^(16::Int)))) p :: Image Pixel16
      putStrLn $ "writing: " ++ path
      putStrLn $ "maximum: " ++ show (V.maximum . imageData $ img)
      putStrLn $ "minimum: " ++ show (V.minimum . imageData $ img)
      writePng path img

--------------------------------------------------


mat2img :: Element (PixelBaseComponent a) 
        => Matrix (PixelBaseComponent a) -> Image a
mat2img m = Image (cols m) (rows m) (flatten m)
img2mat :: S.Storable (PixelBaseComponent a) 
        => Image a -> Matrix (PixelBaseComponent a)
img2mat i = reshape (imageWidth i) $ imageData i


mat2mat' :: Element a => Matrix a -> Mat a
mat2mat' m = Mat . V.convert $ flatten m

mat'2img ::
  S.Storable (PixelBaseComponent a) =>
  Int -> Int -> Mat (PixelBaseComponent a) -> Image a
mat'2img w h m = Image w h (V.convert . unMat $ m)


readCSVMat :: Read a => FilePath -> IO [Mat a]
readCSVMat fp = do
    f <- readFile fp
    let ds =  map read . filter (/= "") . splitWhen (\x -> elem x ("\n," :: String)) $ f
    return $ map (Mat . B.fromList) $ chunksOf 64 ds

readCSVVec :: (UV.Unbox a, Read a) => Int -> FilePath -> IO [UV.Vector a]
readCSVVec n fp = do
    f <- readFile fp
    let ds =  map read . filter (/= "") . splitWhen (\x -> elem x ("\n," :: String)) $ f
    return $ map UV.fromList $ chunksOf n ds

readCSVImage :: (UV.Unbox a, Read a) => Int -> Int -> FilePath -> IO (Mat a)
readCSVImage w h fp = do
    f <- readFile fp
    let ds = map read . filter (/= "") . splitWhen (\x -> elem x ("\n," :: String)) $ f
    return . Mat . V.fromListN (w*h) $ ds


{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c


iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = f a >>= iterateM f
