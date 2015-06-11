
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonadComprehensions #-}

module LearnGabors where


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
{-import           Numeric.FastMath-}

import           Control.Monad.Random
import           Control.Monad.State
import           Control.Monad.Fix
import           Control.Monad.Zip
import           Control.Monad hiding (forM_,mapM,mapM_)

import           Control.DeepSeq (force)
import           Control.Exception (evaluate)

import           Control.Parallel.Strategies

import           Codec.Picture

import           System.Directory


import           GHC.TypeLits


import           Debug.Trace

-----------------------------------------------------------------------------

type Img w h a = Mat w h a
type Phi a     = Mat 8 8 a
type Patch a   = Mat 8 8 a
type As a      = B.Vector a

type Mats w h a = B.Vector (Mat w h a)
type Imgs w h a = B.Vector (Img w h a)
type Phis     a = B.Vector (Phi     a)
type Patches  a = B.Vector (Patch   a)
type Ass      a = B.Vector (As      a)

-------------------------------------------------

reconstruct :: Num a => B.Vector a -> B.Vector (Phi a) -> Patch a
reconstruct as φs = sum [ a `scale` φ | a <- as | φ <- φs ]

sparseness :: Floating a => a -> a -> a
sparseness σ a = s (a / σ) where s x = log (1+x*x)

preserveInfos :: Num a => Patch a -> B.Vector (Phi a) -> As a -> a
preserveInfos patch φs as = sumElems' $ (patch - reconstruct as φs) ^ (2::Int)

errorFunction ::
  Floating a => a -> a -> a -> Patch a -> Phis a -> As a -> a
errorFunction λ β σ patch φs as = λ * preserveInfos patch φs as + bos * sum [ sparseness σ a | a <- as ]
  where bos = β / σ

findAsForImg :: forall a. (Floating a, Ord a)
             => a -> a -> a -> Patch a -> Phis a -> As a -> [As a]
findAsForImg λ β σ patch φs = conjugateGradientDescent go
  where go :: forall t. (Scalar t ~ a, Mode t, Floating t) => B.Vector t -> t
        go = errorFunction (auto λ) (auto β) (auto σ) (auto <$> patch) (auto <$$> φs)

findAsForImg' :: (Floating a, Ord a)
              => a -> a -> a -> Patch a -> B.Vector (Phi a) -> As a -> As a
findAsForImg' λ β σ patch φs = go . findAsForImg λ β σ patch φs
  where go (a:b:xs) | errDiff a b < 0.01 = b
                    | otherwise          = go (b:xs)
        errFun = errorFunction λ β σ patch φs
        errDiff a b = abs (errFun a - errFun b)

{-initialAs :: Fractional a => Patch a -> B.Vector (Phi a) -> As a-}
{-initialAs patch = V.map go-}
{-  where go φ = sumElems' (φ * patch) / norm-}
{-          where norm = sumElems' $ φ^(2::Int)-}

initialAs patches phis = B.fromList [ B.zipWith (/) s normA2 | s <- sinit ]
    where sinit  = map B.fromList . chunksOf 64 . toList 
                 $ [ sumElems'  (patch * phi) | patch <- patches,  phi <- phis ]
          normA2 = [ sumElems' (phi*phi) | phi <- phis ]

residualError :: Num a => Patches a -> Ass a -> Phis a -> B.Vector (Img 8 8 a)
residualError patches ass φs = [ patch - reconstruct as φs | patch <- patches | as <- ass ]

updateBases ::
  (Fractional a, Num a) => B.Vector (Mat w h a) -> B.Vector (As a) -> B.Vector (Mat w h a)
updateBases es ass = V.map (le `scale`) dA
    where dA = V.foldl1' (mzipWith (+)) [[ a `scale` e | a <- as ] | e <- es | as <- ass ]
          le = 1 / batchSize
          batchSize = fromIntegral $ V.length es

-- this code corresponds to line 71-78 in the sparsenet code

varΗ, varGoal :: Double
varΗ = 0.001
varGoal = 0.1

adjustAVar :: B.Vector Double -> As Double -> B.Vector Double
adjustAVar vars as = mzipWith (+) [ var * (1-varΗ) | var <- vars ] [ varΗ * a^(2::Int) | a <- as ]

adjustAVars :: B.Vector Double -> B.Vector (As Double) -> As Double
adjustAVars = foldl' adjustAVar

adjustGain :: Double -> B.Vector  Double -> B.Vector Double -> B.Vector Double
adjustGain α vars gain = V.zipWith (*) gain (fmap ((**α) . (/varGoal)) vars)


adjustPhiVariance :: Fractional a => B.Vector a -> B.Vector a -> B.Vector (Phi a) -> B.Vector (Phi a)
adjustPhiVariance = V.zipWith3 (\g n φ -> (g/n) `scale` φ)

adjustPhisForVariance ::
  MonadState (IterationState Double) m =>
  Double -> B.Vector (As Double) -> m ()
adjustPhisForVariance α ass = do

    -- get old values ... no idea if this is really necessary
    -- but it is done in the olshausen code (probably without him being
    -- aware of it)
    vars <- isVars <$> get
    gain <- isGain <$> get
    phis <- isPhis <$> get

    let vars' = adjustAVars vars ass
        normA = fmap norm phis
        gain' = adjustGain α vars' gain
        phis' = adjustPhiVariance gain' normA phis

    modify' (\s -> s { isVars = vars', isGain = gain', isPhis = phis' })

----


data IterationState a = IS 
    { isVars :: B.Vector a
    , isGain :: B.Vector a
    , isPhis :: B.Vector (Phi a)
    , isIteration :: ! Int
    } deriving (Show)

mkInitialIS φs = IS
    { isVars = V.replicate 64 varGoal
    , isGain = V.map norm φs
    , isPhis = φs
    , isIteration = 1
    }

oneIteration :: (MonadIO m, MonadRandom m, MonadState (IterationState Double) m)
             => Imgs 512 512 Double -> Double -> Double -> Double -> Double -> m ()
oneIteration imgs α β λ σ = do

    -- get iteration
    iteration <- isIteration <$> get
    liftIO $ putStrLn $ "--- iteration: " ++ show iteration ++ " ---"

    -- choose an image for this batch
    img <- uniform $ toList imgs

    {-let imgMin = minimum $ toList img-}
    {-    imgMax = maximum $ toList img-}
    {-liftIO $ putStrLn $ "image min: " ++ show imgMin ++ ", max: " ++ show imgMax-}

    -- get phis
    φs <- isPhis <$> get
    liftIO $ writePhisToPng ("output/phis-" ++ show iteration ++ ".png") (toList φs)

    -- extract subimages at random from this image to make data vector X
    liftIO $ putStrLn $ " → chosing patches"
    patches <- B.replicateM 100 (randomPatch img)
    liftIO $ writePhisToPng ("output/patches-" ++ show iteration ++ ".png") (toList patches)




    -- calculate coefficients for these data via conjugate gradient routine
    liftIO $ putStrLn $ " → calculating coefficients"
    let initAs    = initialAs patches φs -- [ initialAs patch φs | patch <- patches ]
   
    -- force evaluation here
    fittedAs <- liftIO $ evaluate $ force 
              $ withStrategy (parTraversable rdeepseq) 
              $ [ findAsForImg' λ β σ patch φs as  | patch <- patches | as <- initAs ]

    

    -- calculate residual error
    liftIO $ putStrLn $ " → calculating residuals"
    let err = residualError patches fittedAs φs

    liftIO $ writePhisToPng ("output/residual" ++ show iteration ++ ".png") (toList err)

    -- update bases
    let deltaφs = updateBases err fittedAs
        η       = 1

    liftIO $ writePhisToPng ("output/deltaphis-"++ show iteration ++".png") (toList deltaφs)

    liftIO $ putStrLn $ " → modifying φs"
    modify' (\is -> is {isPhis = [ φ + η * dφ | φ <- isPhis is | dφ <- deltaφs ]})



    -- normalize bases
    -- (there is some state hidden here)
    liftIO $ putStrLn $ " → adjusting φs"
    adjustPhisForVariance α fittedAs

    {-nextφs <- isPhis <$> get-}
    {-liftIO $ writePhisToPng ("output/nextphis-"++ show iteration ++".png") (toList nextφs)-}

    {-liftIO $ putStrLn "--- pause ---"-}
    {-s <- liftIO $ readLn-}
    {-liftIO $ putStrLn s-}


learnGabors:: IO ()
learnGabors = do

    -- magic numbers from olshausen code
    let α = 0.02
        noiseVar = 0.01
        β = 2.2
        λ = 1/noiseVar
        σ = 0.316

    -- generate random φs
    putStrLn "generating random φs"
    φs <- evalRandIO $ V.replicateM 64 randomPhi :: IO (Phis Double)
    {-φs <- readCSVMats "data/mats/a.csv" :: IO (Phis Double)-}

    -- read in images
    putStrLn "reading in images"
    let imageNames = [ "data/mats/images/img" ++ show n ++ ".csv" | n <- [0..9::Int] ]
    images <- B.fromList <$> mapM readCSVImage imageNames :: IO (B.Vector (Img 512 512 Double))

    -- initialize state
    let isState = mkInitialIS φs

    let phis = isPhis isState
    writePhisToPng "output/iter-0.png" (toList phis)

    -- run iteration
    gen <- getStdGen
    _ <- evalRandT (execStateT (runLearnGabors images α β λ σ) isState) gen


    {-evalRandIO (execStateT stuff isState)-}

    putStrLn "done"

runLearnGabors ::
  (MonadIO m, MonadState (IterationState Double) m, MonadRandom m) =>
  Imgs 512 512 Double -> Double -> Double -> Double -> Double -> m b
runLearnGabors imgs α β λ σ = forever go
    where go = do
            i <- isIteration <$> get

            oneIteration imgs α β λ σ
            modify' $ \s -> s {isPhis = manifestU <$> isPhis s}


            phis <- isPhis <$> get

            let dir = "output/iter-" ++ show i ++ "/"
            {-liftIO $ createDirectoryIfMissing True dir-}
            {-forM_ (zip [0..] phis) $ \(i,φ) -> -}
            {-    liftIO $ writeCSVImage (dir ++ "phi-" ++ show i ++ ".csv") φ-}

            -- create image from mats 
            {-liftIO $ print (minimum $ fmap minimum phis)-}
            {-liftIO $ print (maximum $ fmap maximum phis)-}
            liftIO $ writePhisToPng ("output/iter-" ++ show i ++ ".png") (toList phis)

            modify' (\s -> s { isIteration = i + 1 })


writePhisToPng fn phis = writePng fn img
  where phis' = map (fmap (floor . (*127) . (+1) . (/mm)) ) phis
        img   = concatMatsInImage phis' :: Image Pixel8
        mm    = max (abs $ minimum idata) (abs $ maximum idata)
        idata = concatMap toList phis

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

randomPhi :: (UV.Unbox a, Num a, Random a, MonadRandom m, MonadFix m) => m (Phi a)
randomPhi = mfix $ \m -> do
    let w = width m
        h = height m
    v <- UV.replicateM (w*h) (getRandomR (-1,1))
    return $ Mat $ \x y -> v UV.! (x + y * w)

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


{-readCSVMat :: forall w h a. (KnownNat w, Read a) => FilePath -> IO [Mat w h a]-}
{-readCSVMat fp = do-}
{-    f <- readFile fp-}
{-    let ds =  map read . filter (/= "") . splitWhen (\x -> elem x ("\n," :: String)) $ f-}
{-        w  = fromInteger $ natVal (Proxy :: Proxy w)-}
{-    -- make sure that we get no half matrices-}
{-    when (length ds `mod` w /= 0) $ error "readCSVMat: dimensions do not match"-}
{-    return $ map (Mat . B.fromList) $ chunksOf w ds-}

readCSVMats :: forall w h a. (KnownNat w, KnownNat h, UV.Unbox a, Read a)
            => FilePath -> IO (Mats w h a)
readCSVMats fp = do
    f <- readFile fp
    let ds =  map read . filter (/= "") . splitWhen (\x -> x `elem` ("\n," :: String)) $ f
        w  = fromInteger $ natVal (Proxy :: Proxy w)
        h  = fromInteger $ natVal (Proxy :: Proxy h)
        s  = w * h
    -- make sure that we get no half matrices
    when (length ds `mod` s /= 0) $ error "readCSVMats: dimensions do not match"
    return . B.fromList . map mkMatU $ chunksOf s ds


readCSVVec :: (UV.Unbox a, Read a) => FilePath -> IO (B.Vector a)
readCSVVec fp = do
    f <- readFile fp
    let ds =  map read . filter (/= "") . splitWhen (\x -> x `elem` ("\n," :: String)) $ f
    return $ B.fromList ds

readCSVVecs :: Read a => FilePath -> IO (B.Vector (B.Vector a))
readCSVVecs fp = do
    f <- readFile fp
    let ls = B.fromList . splitOn "," <$> lines f
        ds = read <$$> ls

    return $ V.fromList ds



{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c


iterateM :: Monad m => (a -> m a) -> a -> m b
iterateM f a = f a >>= iterateM f

instance MonadZip B.Vector where
    mzip = B.zip
    mzipWith = B.zipWith
    munzip = B.unzip

infixl 4 <$$>
(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f0 (f1 a) -> f0 (f1 b)
f <$$> x = fmap (fmap f) x
{-# INLINE (<$$>) #-}
