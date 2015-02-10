


module Data.AER.Synthesize (synthesize) where


import qualified Data.AER.DVS128 as DVS
import qualified Data.AER.Types  as DVS

import           System.Random

import           Data.Thyme.Clock
import           Data.List
import           Data.Monoid
import           Data.Functor
import           Data.Function

import           Control.Applicative
import           Control.Monad.Random

{-randomPosition :: RandomGen g-}
{-               => (NominalDiffTime, NominalDiffTime)-}
{-               -> ((NominalDiffTime, Int, Int), g)-}
{-randomPosition bounds g0 = ((rt,rx,ry),g3)-}
{-    where (rt,g1) = randomR bounds g0-}
{-          (rx,g2) = randomR (0,128) g1-}
{-          (ry,g3) = randomR (0,128) g2-}

testEvent distFun time posX posY p = p < distFun time posX posY


acceptReject dist ts xs ys pols ps = map snd 
                                   . filter fst 
                                   $ zipWith5 (\t x y pol p -> (p < dist t x y pol,(t,x,y,pol))) ts xs ys pols ps

synthesize :: Int                                                       -- ^ number of events
           -> (NominalDiffTime, NominalDiffTime)                        -- ^ bounds
           -> (NominalDiffTime -> Int -> Int -> DVS.Polarity -> Double) -- ^ probability distribution
           -> IO [DVS.Event DVS.Address] -- ^ infinite list taken from distribution
synthesize num bounds distFun = do

    let maxNum = 10 * num

    rts <- take maxNum <$> getRandomRs bounds
    rxs <- take maxNum <$> getRandomRs (0,128)
    rys <- take maxNum <$> getRandomRs (0,128)
    pols <- take maxNum . map (\b -> if b then DVS.U else DVS.D) <$> getRandoms
    ps  <- take maxNum <$> getRandomRs (0,1)

    let preEs = acceptReject distFun rts rxs rys pols ps

    return $ sortBy (compare `on` DVS.timestamp)
           $ take num
           $ map (\(t,x,y,p) -> DVS.Event (DVS.Address p (fromIntegral x) (fromIntegral y)) t)
           $ preEs






testDist :: NominalDiffTime -> Int -> Int -> DVS.Polarity -> Double
testDist t x y pol = sin (toSeconds t) * sin (fromIntegral x/10) * cos (fromIntegral y/10)

{-synthesize bounds distFun g =-}
{-    where rpos = randomPositions bounds g-}

