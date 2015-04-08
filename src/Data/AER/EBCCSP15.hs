
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Main  where


import           Data.Function
import           Data.Traversable
import           Data.Foldable
import           Data.List.Split

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector          as B

import           Numeric.LinearAlgebra.HMatrix
import           Numeric.AD

import           Control.Applicative
import           Control.Monad.Random
import           Control.Monad hiding (forM_,mapM,mapM_)
import           Control.Parallel.Strategies

import           Codec.Picture


import           Debug.Trace

import           Prelude hiding (forM_,mapM,mapM_,sum,foldl1)



infixl 4 <$$>
f <$$> x = fmap (fmap f) x


-----------------------------------------------------------------------------

newtype Mat a = Mat { unMat :: B.Vector a } deriving (Functor)

scale' f = Mat . V.map (*f) . unMat
add' a b = Mat $ V.zipWith (+) (unMat a) (unMat b)
sub' a b = Mat $ V.zipWith (-) (unMat a) (unMat b)
mul' a b = Mat $ V.zipWith (*) (unMat a) (unMat b)
pow' a e = Mat $ V.map (^e) (unMat a)

sumElems' :: Num a => Mat a -> a
sumElems' = V.sum . unMat


eq1' :: Num a => [a] -> [Mat a] -> Mat a
eq1' as φs = foldl1 add' $ zipWith scale' as φs

eq3' :: Num a => Mat a -> [Mat a] -> [a] -> a
eq3' img φs as = negate $ sumElems' (errImg `pow'` (2::Int))
  where errImg = img `sub'` (eq1' as φs)

diffTest :: forall a. (Fractional a, Floating a, Ord a) => Mat a -> [Mat a] -> [a] -> [[a]]
diffTest m φs as0 = gradientDescent go as0
    where go :: forall t. (Scalar t ~ a, Mode t, Floating t) => [t] -> t
          go as = - eq3' (auto <$> m) (auto <$$> φs) as - 0.14 * eq4a 0.14 as

-----------------------------------------------------------------------------

-- equation one: add up base functions and factors

eq1 :: (Container m a, Num (m a)) => [a] -> [m a] -> m a
eq1 as φs = sum [ a `scale` φ | a <- as | φ <- φs ]

eq3 :: (Container m a, Num (m a)) => m a -> [a] -> [m a] -> a
eq3 img as φs = - sumElements (errImg * errImg)
    where errImg = img - (eq1 as φs)

eq4a :: Floating a => a -> [a] -> a
eq4a σ as = - sum [ eqs (a/σ) | a <- as ]

eqs x = log (1+x*x)
eqs' x = (2*x) / (x*x+1)


eq5 λ σ img φs as φ a = b - c - s
    where b = sumElements (φ * img)
          c = sum [ aj * (sumElements (φ * φj)) | aj <- as | φj <- φs ]
          s = (λ/σ) * eqs' (a / σ)

eq5s λ σ img φs as = [ eq5 λ σ img φs as φ a | φ <- φs | a <- as ]

eq6 η img as φs a = η `scale` ( a `scale` (img * eq1 as φs))

adEq5 :: forall m a. (Container m a, Floating a)
      => m a -> [m a] -> [a] -> [[a]]
adEq5 m φs as0 = gradientDescent go as0
  where go :: forall t. (Scalar t ~ a, Mode t, Floating t) => [t] -> t
        go as = - eq3 (auto <$> m) (auto <$$> φs) as - 0.14 * eq4a 0.14 as

main :: IO ()
main = do


    let imgPath = "../../data/swiss/swiss0-white.png"

    putStrLn $ "create 192 16x16 matrices"
    phis <- map single <$> replicateM 192 (randn 16 16)
  
    putStrLn $ "create 192 values"
    as <- replicateM 192 randomIO :: IO [Float]

    putStrLn $ "read image"
    img <- readAsMat imgPath

    putStrLn $ "draw random patches"
    patches <- replicateM 100 (randomPatch 16 img)


    -- just testing:
    -- convert to mat format
    let ps = map (Mat . V.convert . flatten) phis
        i  = Mat . V.convert . flatten $ head patches

    let ds = diffTest i ps as

    mapM_ print $ take 500 ds


    putStrLn "done"



--------------------------------------------------

readAsMat :: FilePath -> IO (Matrix Float)
readAsMat fn = do
    (Right (ImageY16 img)) <- readImage fn
    let f = pixelMap ((/(2^^16-1)) . fromIntegral) $ img :: Image Float
    return $ img2mat f

randomPatch s m = do
    let t = s `div` 2
    c <- randomRIO (t, cols m - t)
    r <- randomRIO (t, rows m - t)

    return $ subMatrix (r-t,c-t) (s,s) m

createRandomPatchesFromImage imagePath = do
    -- read in test image
    putStrLn "reading image"
    (Right (ImageY16 raw)) <- readPng imagePath

    -- convert in matrix
    let img :: Image Float
        img = pixelMap ( (/(2^^16-1)) . fromIntegral ) raw
        mat = img2mat img

    -- draw random submatrices
    ms <- replicateM 512 (randomPatch 16 mat)
    let ps = map mat2img ms :: [Image PixelF]


    -- write out patches as images
    forM_ (zip [0..] ps) $ \(i,p) -> do
      let path = "/tmp/imgs/img" ++ show i ++ ".png"
          img  = pixelMap (round . (*(2^16))) p :: Image Pixel16
      putStrLn $ "writing: " ++ path
      putStrLn $ "maximum: " ++ show (V.maximum . imageData $ img)
      putStrLn $ "minimum: " ++ show (V.minimum . imageData $ img)
      writePng path img

--------------------------------------------------


mat2img m = Image (cols m) (rows m) (flatten m)
img2mat i = reshape (imageWidth i) $ imageData i
