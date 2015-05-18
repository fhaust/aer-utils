
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where


import           Data.Foldable

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector          as B

import           Data.List
import           Data.List.Split

import           Data.Proxy
import           Data.SimpleMat

import           Numeric.AD

import           Control.Monad.Random
import           Control.Monad.State
import           Control.Monad.Fix
import           Control.Monad hiding (forM_,mapM,mapM_)

import           Codec.Picture

import           System.Directory


import           GHC.TypeLits


import           Debug.Trace




infixl 4 <$$>
(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f0 (f1 a) -> f0 (f1 b)
f <$$> x = fmap (fmap f) x


-----------------------------------------------------------------------------

type Img w h a = Mat w h a
type Phi w h a = Mat w h a

-------------------------------------------------

reconstruct :: Num a => [a] -> [Phi w h a] -> Img w h a
reconstruct as φs = sum [ a `scale` φ | a <- as | φ <- φs ]

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
updateBases es ass = map (le `scale`) dA
    where dA = foldl1' (zipWith (+)) [[ a `scale` e | a <- as ] | e <- es | as <- ass ]
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
adjustPhiVariance gains norms φs = zipWith3 (\g n φ -> (g/n) `scale` φ) gains norms φs

adjustPhisForVariance ::
  MonadState (IterationState Double) m =>
  Double -> [[Double]] -> [Mat 8 8 Double] -> m ()
adjustPhisForVariance α ass φs = do

    -- get old values ... no idea if this is really necessary
    -- but it is done in the olshausen code (probably without him being
    -- aware of it)
    vars <- isVars <$> get
    gain <- isGain <$> get

    let vars' = adjustAVars vars ass
        normA = map norm φs
        gain' = adjustGain α vars' gain
        phis' = adjustPhiVariance gain' normA φs

    modify' (\s -> s { isVars = vars', isGain = gain', isPhis = phis' })

----


data IterationState a = IS 
    { isVars :: ! [a]
    , isGain :: ! [a]
    , isImgs :: ! [Img 512 512 a]
    , isPhis :: ! [Phi 8 8 a]
    , isIteration :: ! Int
    }

mkInitialIS imgs φs = IS
    { isVars = replicate 64 varGoal
    , isGain = map norm φs
    , isImgs = imgs
    , isPhis = φs
    , isIteration = 0
    }

oneIteration :: (MonadIO m, MonadRandom m, MonadState (IterationState Double) m)
             => Double -> Double -> Double -> m ()
oneIteration α λ σ = do

    -- choose an image for this batch
    imgs <- isImgs <$> get
    img <- uniform imgs

    -- get phis
    φs <- isPhis <$> get

    -- extract subimages at random from this image to make data vector X
    patches <- replicateM 100 (randomPatch img)

    let ps' = map (fmap (*0.5).(+1)) patches
    liftIO $ writePhisToPng "output/cur-patches.png" ps'


    -- calculate coefficients for these data via conjugate gradient routine
    let initAs    = [ initialAs patch φs | patch <- patches ]
        fittedAs  = [ findAsForImg λ σ patch φs as !! 10 | patch <- patches | as <- initAs ]

    -- calculate residual error
    let err = residualError patches fittedAs φs

    -- update bases
    let deltaφs = updateBases err fittedAs

    -- normalize bases
    -- (there is some state hidden here)
    adjustPhisForVariance α fittedAs deltaφs



main :: IO ()
main = do

    -- magic numbers from olshausen code
    let α = 0.02
        λ = 100
        σ = 0.316

    -- generate random φs
    {-φs <- evalRandIO $ replicateM 64 randomPhi :: IO [Phi 8 8 Double]-}
    φs <- readCSVMats "data/mats/a.csv" :: IO [Phi 8 8 Double]

    -- read in images
    let imageNames = [ "data/mats/images/img" ++ show n ++ ".csv" | n <- [0..9::Int] ]
    images <- mapM readCSVImage imageNames :: IO [Img 512 512 Double]

    -- initialize state
    let isState = mkInitialIS images φs
            
    let phis = isPhis isState
    writePhisToPng "output/iter-X.png" phis

    -- run iteration
    gen <- getStdGen
    _ <- evalRandT (execStateT (runLearnGabors α λ σ) isState) gen

    
    {-evalRandIO (execStateT stuff isState)-}

    putStrLn "done"

runLearnGabors α λ σ = forever go
    where go = do
            i <- isIteration <$> get
            liftIO $ putStrLn $ "iteration: " ++ show i

            oneIteration α λ σ

            phis <- isPhis <$> get

            let dir = "output/iter-" ++ show i ++ "/"
            {-liftIO $ createDirectoryIfMissing True dir-}
            {-forM_ (zip [0..] phis) $ \(i,φ) -> -}
            {-    liftIO $ writeCSVImage (dir ++ "phi-" ++ show i ++ ".csv") φ-}

            -- create image from mats 
            liftIO $ writePhisToPng ("output/iter-" ++ show i ++ ".png") phis

            modify' (\s -> s { isIteration = i + 1 })


writePhisToPng fn phis = writePng fn img
  where phis' = map (fmap (floor . (*255))) phis
        img   = concatMatsInImage phis' :: Image Pixel8

concatMatsInImage :: forall w h a. (KnownNat w, KnownNat h, Pixel a)
                  => [Mat w h a] -> Image a
concatMatsInImage ms = generateImage (index m) 64 64
    where rs = map concatHor . chunksOf 8 $ ms :: [Mat 64 8 a]
          m  = concatVer rs :: Mat 64 64 a



--------------------------------------------------

--readAsMat :: FilePath -> IO (Matrix Float)
--readAsMat fn = do
--    (Right (ImageY16 img)) <- readImage fn
--    let f = pixelMap ((/(2^^(16::Int)-1)) . fromIntegral) img :: Image Float
--    return $ img2mat f

randomPatch :: forall w h a w' h' m. (KnownNat w, KnownNat h, KnownNat w', KnownNat h', MonadRandom m)
            => Mat w h a -> m (Mat w' h' a)
randomPatch m = do
    let w' = fromInteger $ natVal (Proxy :: Proxy w')
        h' = fromInteger $ natVal (Proxy :: Proxy h')

    -- get random offsets
    ox <- getRandomR (0, width m  - w' - 1)
    oy <- getRandomR (0, height m - h' - 1)

    -- return sub matrix
    return $ subMat ox oy m

randomPhi :: (KnownNat w, KnownNat h, Random a, MonadRandom m, MonadFix m) => m (Mat w h a)
randomPhi = mfix $ \m -> do
    let s = width m * height m
    Mat <$> V.replicateM s getRandom

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

readCSVMats :: forall w h a. (KnownNat w, KnownNat h, Read a) => FilePath -> IO [Mat w h a]
readCSVMats fn = do
    f <- readFile fn
    let ls = lines f
        ms = map (mkMat . map read . splitOn "," ) ls
    return ms


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


{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c


iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = f a >>= iterateM f
