
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           OlshausenOnStreams

main :: IO ()
main = do
    OlshausenOnStreams.test
    putStrLn "done"

{-convertImage from to = do-}
{-    f <- readFile from-}
{-    let ss = map (U.fromList . map read . splitOn ",") $ lines f :: [U.Vector Double]-}
{-        i  = generateImage (\x y -> realToFrac ((ss !! y) U.! x)) 512 512 :: Image PixelF-}
{-    writeHDR to (promoteImage i)-}










{-normalizeMat m = fmap (\a -> (a - low) / (high - low)) m-}
{-    where low = minimum m-}
{-          high = maximum m-}


{-createTimeFronts :: NominalDiffTime -> [DVS.Event DVS.Address] -> [M.Mat 128 128 Float]-}
{-createTimeFronts windowSize es = ts-}

{-    where ts = map ( normalizeMat  -- normalize to window sizes-}
{-                   . M.mkMatU . U.toList . U.map (realToFrac . (`mod'` windowSize) . fst) -- convert to matrix-}
{-                   . timeFront           -- create timefronts-}
{-                   )-}
{-             . filter ((>50) . length) -- only use windows with at least some content-}
{-             . extractWindows windowSize -}
{-             $ es -}



{-timeFrontToImage :: (KnownNat w, KnownNat h, RealFrac a) => M.Mat w h a -> Image Pixel16-}
{-timeFrontToImage ts = generateImage go (M.width ts) (M.height ts)-}
{-    where go x y = round . (*2^16) $ M.index ts x y-}
