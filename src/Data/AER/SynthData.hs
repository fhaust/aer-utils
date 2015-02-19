


module Data.AER.SynthData where


import           Codec.Picture
import           Codec.Picture.Types

import           Data.AER.DVS128
import           Data.AER.Types
import           Data.Word7

import           Data.Function
import           Data.List

import           Data.Thyme.Clock

import           Control.Monad
import           Control.Applicative
import           Control.Monad.Random

{-synthLine :: IO (Image Pixel8)-}
{-synthLine = do-}


{-  (Right (ImageRGBA8 white)) <- readImage "../aer/data/synth/empty-128-128.png"-}
{-  (Right (ImageRGBA8 beam))  <- readImage "../aer/data/synth/beam-7-128-128.png"-}

{-  let diff = (flip (flip generateImage 128) 128) $ \x y -> (computeLuma $ pixelAt white x y) - (computeLuma $ pixelAt beam  x y)-}

{-  return diff-}
{-  [>return $ (flip dynamicMap) white $ \w -><]-}
{-  [>           (flip dynamicMap) beam $ \b -><]-}
{-  [>             generateImage (\x y -> pixelAt w x y - pixelAt b x y) 128 128<]-}

{-blockDist t x y p | t < 1            = 0-}
{-                  | p /= U           = 0-}
{-                  | x < 30 || (x > 34 && x < 94) || x > 98 = 0-}
{-                  | y < 32 || y > 96 = 0-}
{-                  | otherwise        = 0.9-}
blockDist t x y p | t < 1            = 0
                  | p == U, x < 64 && x > 60 = 0.9
                  | p == D, x > 64 && x < 70 = 0.9
                  | otherwise        = 0.0

postPro = sortBy (compare `on` timestamp) . concat

synthesize :: NominalDiffTime -> NominalDiffTime -> Int
           -> (NominalDiffTime -> Word7 -> Word7 -> Polarity -> Double)
           -> IO [Event Address]
synthesize startT endT count dist = fmap postPro $ replicateM count $ do
    t <- getRandomR (startT,endT)
    x <- Word7 <$> getRandomR (0,127)
    y <- Word7 <$> getRandomR (0,127)
    p <- uniform [U,D]

    let d = dist t x y p

    r <- getRandomR (0,1::Double)

    return $ if r < d then [Event (Address p x y) t] else []


