
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main  where


import           Data.Foldable

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector          as B

import           Numeric.LinearAlgebra.HMatrix
import           Numeric.AD

import           Control.Monad.Random
import           Control.Monad hiding (forM_,mapM,mapM_)

import           Codec.Picture


{-import           Debug.Trace-}




infixl 4 <$$>
(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f0 (f1 a) -> f0 (f1 b)
f <$$> x = fmap (fmap f) x


-----------------------------------------------------------------------------

newtype Mat a = Mat { unMat :: B.Vector a } deriving (Functor)

scale' :: Num a => a -> Mat a -> Mat a 
scale' f = Mat . V.map (*f) . unMat
add' :: Num a => Mat a -> Mat a -> Mat a
add' a b = Mat $ V.zipWith (+) (unMat a) (unMat b)
sub' :: Num a => Mat a -> Mat a -> Mat a
sub' a b = Mat $ V.zipWith (-) (unMat a) (unMat b)
mul' :: Num a => Mat a -> Mat a -> Mat a
mul' a b = Mat $ V.zipWith (*) (unMat a) (unMat b)
pow' :: (Integral b, Num a) => Mat a -> b -> Mat a
pow' a e = Mat $ V.map (^e) (unMat a)

sumElems' :: Num a => Mat a -> a
sumElems' = V.sum . unMat


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

diffTest :: forall a. (Fractional a, Floating a, Ord a) => Mat a -> [Mat a] -> [a] -> [[a]]
diffTest m φs = gradientDescent go
    where go :: forall t. (Scalar t ~ a, Mode t, Floating t) => [t] -> t
          go = errFun (auto <$> m) (auto <$$> φs)


-- | take the output of diffTest and give back the first result where
-- the error functions change is below "err" 
diffTill :: (Floating a, Ord a) => a -> Mat a -> [Mat a] -> [[a]] -> (a,[a])
diffTill err m φs ass = head $ dropWhile ((>err).fst) [ (e,as) | e <- es | as <- ass ]
    where es = [ abs (errFun m φs as - errFun m φs bs) | as <- ass | bs <- tail ass ]

-----------------------------------------------------------------------------

-- equation one: add up base functions and factors

{-eq1 :: (Container m a, Num (m a)) => [a] -> [m a] -> m a-}
{-eq1 as φs = sum [ a `scale` φ | a <- as | φ <- φs ]-}

{-eq3 :: (Container m a, Num (m a)) => m a -> [a] -> [m a] -> a-}
{-eq3 img as φs = - sumElements (errImg * errImg)-}
{-    where errImg = img - (eq1 as φs)-}

{-eq4a :: Floating a => a -> [a] -> a-}
{-eq4a σ as = - sum [ eqs (a/σ) | a <- as ]-}

{-eqs x = log (1+x*x)-}
{-eqs' x = (2*x) / (x*x+1)-}


{-eq5 λ σ img φs as φ a = b - c - s-}
{-    where b = sumElements (φ * img)-}
{-          c = sum [ aj * (sumElements (φ * φj)) | aj <- as | φj <- φs ]-}
{-          s = (λ/σ) * eqs' (a / σ)-}

{-eq5s λ σ img φs as = [ eq5 λ σ img φs as φ a | φ <- φs | a <- as ]-}

{-eq6 η img as φs a = η `scale` ( a `scale` (img * eq1 as φs))-}

{-adEq5 :: forall m a. (Container m a, Floating a, Ord a)-}
{-      => m a -> [m a] -> [a] -> [[a]]-}
{-adEq5 m φs as0 = gradientDescent go as0-}
{-  where -- go :: forall t. (Scalar t ~ a, Mode t, Floating t, Element t, Num (m t), Container m t) -}
{-        --   => [t] -> t-}
{-        go :: forall t. (Scalar t ~ a, Mode t, Element t) => [t] -> t-}
{-        go as = - eq3 (cmap auto m) as (fmap (cmap auto) φs)  - 0.14 * eq4a 0.14 as-}

main :: IO ()
main = do


    let imgPath = "../../data/swiss/swiss0-white.png"

    putStrLn "create 192 16x16 matrices"
    phis <- map single <$> replicateM 192 (randn 16 16)
  
    putStrLn "create 192 values"
    as <- replicateM 192 randomIO :: IO [Float]

    putStrLn "read image"
    img <- readAsMat imgPath

    putStrLn "draw random patches"
    patches <- replicateM 100 (randomPatch 16 img)


    -- just testing:
    -- convert to mat format
    let phis'    = map mat2mat' phis
    let patches' = concat . repeat . map mat2mat' $ patches


    let results = iterate (uncurry3 (adjustPhis 0.14 0.8)) (patches',as,phis')
    let (_,as'',phis'') = results !! 10
  
    let imgs' = [ mat'2img 16 16 (xs `scale'` φs) :: Image Float | xs <- as'' | φs <- phis'' ]

    forM_ (zip [(0::Int)..] imgs') $ \(i,im) -> do
      let fn = "/tmp/imgs/i" ++ show i ++ ".png"
          img' = pixelMap (floor . (*255)) im :: Image Pixel8
      writePng fn img'

    putStrLn "done"


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
          φs'        = [ φ `sub'` eq6 η r φs as' a | a <- as' | φ <- φs ]

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


{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c
