
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SimpleMat where


import           GHC.TypeLits
import           GHC.Stack

import qualified Data.Vector as V

import           Data.Proxy
import           Data.List
import           Data.List.Split

{-import           Debug.Trace-}


newtype Mat (w :: Nat) (h :: Nat) a = Mat { unMat :: V.Vector a } deriving (Show,Read)

instance Functor (Mat w h) where
    fmap f (Mat v) = Mat $ V.map f v
    {-# INLINABLE fmap #-}

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
    {-# INLINABLE fromInteger #-}

scale :: Num a => a -> Mat w h a -> Mat w h a 
scale f = Mat . V.map (*f) . unMat
{-# INLINABLE scale #-}

norm :: Floating c => Mat t t1 c -> c
norm (Mat a) = sqrt . V.sum $ V.zipWith (*) a a
{-# INLINABLE norm #-}

variance :: Fractional a => Mat w h a -> a
variance (Mat a) = vecVariance a


vecVariance :: Fractional a => V.Vector a -> a
vecVariance a = V.sum (V.map (\x -> (x - mean)^(2::Int)) a) / len
  where mean = vecMean a
        len  = fromIntegral $ V.length a

vecMean :: Fractional a => V.Vector a -> a
vecMean a = V.sum a / fromIntegral (V.length a)

sumElems' :: Num a => Mat w h a -> a
sumElems' = V.sum . unMat
{-# INLINABLE sumElems' #-}

zipWith' :: (a -> b -> c) -> Mat w h a -> Mat w h b -> Mat w h c
zipWith' f (Mat a) (Mat b) = Mat $ V.zipWith f a b
{-# INLINABLE zipWith' #-}

fromList' :: [a] -> Mat w h a
fromList' l = Mat $ V.fromList l

mkMat :: forall w h a. (KnownNat w, KnownNat h) => [a] -> Mat w h a
mkMat as | s == length as = m
         | otherwise      = error "invalid matrix size"
  where m = Mat $ V.fromListN s as
        s = width m * height m
{-# INLINABLE mkMat #-}

width :: forall w h a. KnownNat w => Mat w h a -> Int
width _ = fromInteger $ natVal (Proxy :: Proxy w)
{-# INLINABLE width #-}

height :: forall w h a. KnownNat h => Mat w h a -> Int
height _ = fromInteger $ natVal (Proxy :: Proxy h)
{-# INLINABLE height #-}

index :: forall w h a. KnownNat w => Mat w h a -> Int -> Int -> a
index m@(Mat v) x y = v V.! ( x + y * width m)
{-# INLINABLE index #-}



row :: KnownNat w => Int -> Mat w h a -> V.Vector a 
row y m@(Mat v) = V.slice (y*w) w v
  where w = width m
{-# INLINABLE row #-}

rows :: (KnownNat w, KnownNat h) => Mat w h a -> [V.Vector a]
rows m@(Mat _) = [ row y m | y <- [0..h-1]]
  where h = height m
{-# INLINABLE rows #-}

subMat :: (KnownNat w, KnownNat h, KnownNat w', KnownNat h') 
       => Int -> Int -> Mat w h a -> Mat w' h' a
subMat ox oy m@(Mat v) | valid     = newM
                       | otherwise = errorWithStackTrace $ "invalid offset or size: " ++ show (ox,oy,newW,newH)
    where newM = Mat $ V.concat [ V.slice (y*oldW+ox) newW v | y <- [oy..oy+newH-1]]
          valid = ox + newW <= oldW && oy + newH <= oldH
          newW = width newM
          newH = height newM
          oldW = width m
          oldH = height m
{-# INLINABLE subMat #-}

--------------------------------------------------

-- FIXME no static guarantees here yet

concatHor :: forall w h w' h' a. (KnownNat w, KnownNat h) 
          => [Mat w h a] -> Mat w' h' a
concatHor = Mat . V.concat . foldl1' (zipWith (V.++)) . map rows

concatVer :: [Mat w h a] -> Mat w' h' a
concatVer = Mat . V.concat . map unMat

--------------------------------------------------

readCSVImage :: forall w h a. (Read a, KnownNat w, KnownNat h)
             => FilePath -> IO (Mat w h a)
readCSVImage fp = do
    f <- readFile fp
    let w  = fromInteger $ natVal (Proxy :: Proxy w)
        h  = fromInteger $ natVal (Proxy :: Proxy h)
        ds = map read . filter (/= "") . splitWhen (\x -> elem x ("\n," :: String)) $ f
    return . Mat . V.fromListN (w*h) $ ds

writeCSVImage ::
  (Show a, KnownNat w, KnownNat h) => FilePath -> Mat w h a -> IO ()
writeCSVImage fp m = writeFile fp $ unlines $ map (intercalate "," . map show . V.toList) $ rows m
