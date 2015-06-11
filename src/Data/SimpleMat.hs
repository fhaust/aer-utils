
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.SimpleMat where


import           GHC.TypeLits
import           GHC.Stack
import           GHC.Exts

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as CSV

import           Data.Proxy
import           Data.List
import           Data.List.Split

import           Control.Monad.Zip
import           Control.Monad

import           Codec.Picture
import           Codec.Picture.Types

{-import           Debug.Trace-}


newtype Mat (w :: Nat) (h :: Nat) a where
    Mat :: (Int -> Int -> a) -> Mat w h a

index :: Mat w h a -> Int -> Int -> a
index (Mat ix) = ix
{-# INLINE index #-}

instance (KnownNat w, KnownNat h, Show a) => Show (Mat w h a) where
    show m = "fromList " ++ show (toList m)


instance Functor (Mat w h) where
    fmap f (Mat ix) = Mat $ \x y -> f $ ix x y
    {-# INLINE fmap #-}

instance Num a => Num (Mat (w :: Nat) (h :: Nat) a) where
    (+) (Mat f) (Mat g) = Mat $ \x y -> f x y + g x y
    (-) (Mat f) (Mat g) = Mat $ \x y -> f x y - g x y
    (*) (Mat f) (Mat g) = Mat $ \x y -> f x y * g x y
    negate (Mat f)      = Mat $ \x y -> negate $ f x y
    abs    (Mat f)      = Mat $ \x y -> abs $ f x y
    signum (Mat f)      = Mat $ \x y -> signum $ f x y
    fromInteger a       = Mat $ \_ _ -> fromInteger a
    {-# INLINE (+) #-}
    {-# INLINE (-) #-}
    {-# INLINE (*) #-}
    {-# INLINE negate #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}

instance (KnownNat w, KnownNat h) => IsList (Mat w h a) where
    type Item (Mat w h a) = a
    fromList ls = m
      where m = Mat $ \x y -> ls !! (x + y * w)
            w = width m
    toList m = [ index m x y | (x,y) <- indices m ]

instance (KnownNat w, KnownNat h) => Foldable (Mat w h) where
    foldMap f m = foldMap f . toList $ m
    foldr f i m = foldr f i . toList $ m

indices m = [ (x,y) | y <- [0..w-1] , x <- [0..h-1] ]
    where w = width m
          h = height m
{-# INLINE indices #-}

scale :: Num a => a -> Mat w h a -> Mat w h a 
scale f (Mat ix) = Mat $ \x y -> f * ix x y
{-# INLINE scale #-}

norm :: (KnownNat w, KnownNat h, Floating a) => Mat w h a -> a
norm m = sqrt . sum $ (m * m)
{-# INLINE norm #-}

variance :: (KnownNat w, KnownNat h, Fractional a) => Mat w h a -> a
variance as = acc / len
  where mean      = genericMean as
        (acc,len) = foldl' (\(s,c) a -> (s + (a-mean)^(2::Int),c+1)) (0,0) as
{-# INLINE variance #-}

vecMean :: Fractional a => V.Vector a -> a
vecMean a = V.sum a / fromIntegral (V.length a)
{-# INLINE vecMean #-}

genericMean :: (Foldable t, Fractional a) => t a -> a
genericMean as = acc / len
    where (acc,len) = foldl' (\(s,c) a -> (s+a,c+1)) (0,0) as
{-# INLINE genericMean #-}

sumElems' :: (KnownNat w, KnownNat h, Num a) => Mat w h a -> a
sumElems' = sum
{-# INLINE sumElems' #-}

zipWith :: (a -> b -> c) -> Mat w h a -> Mat w h b -> Mat w h c
zipWith f (Mat g) (Mat h) = Mat $ \x y -> f (g x y) (h x y)
{-# INLINE zipWith #-}

mkMatU :: forall w h a. (KnownNat w, KnownNat h, U.Unbox a) => [a] -> Mat w h a
mkMatU as | w*h == length as = m
          | otherwise      = error "invalid matrix size"
  where m = Mat $ \x y -> (U.fromListN (w*h) as) U.! (x + y * w)
        w = width m
        h = height m
{-# INLINE mkMatU #-}

manifestU :: (KnownNat w, U.Unbox a) => Mat w h a -> Mat w h a
manifestU m@(Mat f) = Mat $ \x y -> v U.! (x + y * w)
    where w = width m
          h = width m
          v = U.generate (w*h) (\i -> let (y,x) = i `divMod` w in f x y)
{-# INLINE manifestU #-}

width :: forall w h a. KnownNat w => Mat w h a -> Int
width _ = fromInteger $ natVal (Proxy :: Proxy w)
{-# INLINE width #-}

height :: forall w h a. KnownNat h => Mat w h a -> Int
height _ = fromInteger $ natVal (Proxy :: Proxy h)
{-# INLINE height #-}

row :: KnownNat w => Int -> Mat w h a -> Mat w 1 a
row y (Mat f) = Mat $ \x _ -> f x y
{-# INLINE row #-}

rows :: (KnownNat w, KnownNat h) => Mat w h a -> [Mat w 1 a]
rows m@(Mat _) = [ row y m | y <- [0..h-1]]
  where h = height m
{-# INLINE rows #-}

subMat :: (KnownNat w, KnownNat h, KnownNat w', KnownNat h') 
       => Int -> Int -> Mat w h a -> Mat w' h' a
subMat ox oy m@(Mat f) | valid     = newM
                       | otherwise = errorWithStackTrace $ "invalid offset or size: " ++ show (ox,oy,newW,newH)
    where newM = Mat $ \x y -> f (x+ox) (y+oy)
          valid = ox + newW <= oldW && oy + newH <= oldH
          newW = width newM
          newH = height newM
          oldW = width m
          oldH = height m
{-# INLINE subMat #-}

--------------------------------------------------

-- FIXME no static guarantees here yet

concatHor :: forall w h w' h' a. KnownNat w => [Mat w h a] -> Mat w' h' a
concatHor ms = Mat $ \x y -> let w       = fromInteger $ natVal (Proxy :: Proxy w) 
                                 (dx,mx) = x `divMod` w 
                             in index (ms !! dx) mx y

concatVer :: forall w h w' h' a. KnownNat h => [Mat w h a] -> Mat w' h' a
concatVer ms = Mat $ \x y -> let h = fromInteger $ natVal (Proxy :: Proxy h)
                                 (dy,my) = y `divMod` h
                             in index (ms !! dy) x my

--------------------------------------------------

readCSVImage :: forall w h a. (KnownNat w, KnownNat h, U.Unbox a, CSV.FromField a)
             => FilePath -> IO (Mat w h a)
readCSVImage fp = do
    (Right csv) <- CSV.decode CSV.NoHeader <$> BL.readFile fp
    let m = Mat $ \x y -> csv V.! y U.! x
        w = width m
        h = height m

    unless (V.length csv == h)
      $ errorWithStackTrace ("height " ++ show (V.length csv) ++ "doesn't match type " ++ show h)
    unless (all (== w) (U.length <$> csv))
      $ errorWithStackTrace ("at least one width doesn't match type " ++ show w)

    return m

writeCSVImage ::
  (Show a, KnownNat w, KnownNat h) => FilePath -> Mat w h a -> IO ()
writeCSVImage fp m = writeFile fp $ unlines $ map (intercalate "," . map show . toList) $ rows m


{-readHDRImg :: (KnownNat w, KnownNat h) => FilePath -> IO (Mat w h PixelF)-}
{-readHDRImg fp = do-}
{-    (Right (ImageRGBF rgbi)) <- readHDR fp-}
{-    let i  = extractLumaPlane rgbi :: Image PixelF-}
{-        iw = imageWidth i-}
{-        ih = imageHeight i-}
{-        m  = Mat $ \x y -> realToFrac $ pixelAt i x y-}
{-        mw = width m-}
{-        mh = height m-}

{-    when (mw /= iw || mh /= ih) $ errorWithStackTrace "image dimensions don't match"-}

{-    return m-}

{-writeHDRImg fp m = writeHDR fp img-}
{-    where img = generateImage (index m) (width m) (height m)-}
