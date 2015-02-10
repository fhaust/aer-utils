
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import           GA

import qualified Data.AER.DVS128      as DVS
import qualified Data.AER.Types       as DVS
import           Data.AER.Synthesize

import           Data.Function
import           Data.DTW
import           Data.Bits
import           Data.Complex

import           Data.Thyme.Clock

import qualified Data.Vector.Unboxed as V

import           Control.Applicative
import           Control.Monad.Random
import           Control.Monad
import           Control.Parallel.Strategies


import           System.Random
import           System.Remote.Monitoring

import           Debug.Trace


-- create type for gabor filter + factor


--binaryGauss x x0 σ = case σ of
--                        True  -> 0.5
--                        False -> case x `xor` x0 of
--                                   True  -> 0
--                                   False -> 1



gabor3d
  :: RealFloat a =>
     a -> a -> a -> a -> a -> a -> a -> a -> a -> Complex a
gabor3d λ θ ψ σ γ μ t x y = exp ( (-0.5) * ((t^two) / (μ^two)) :+ 0 ) * exp ( (-0.5) * ((x'^two + γ^two*y'^two) / (σ^two)) :+ 0) * exp ( 0 :+ (2*pi*(x'/λ+ψ)) )
    where x' =  x * cos θ + y * sin θ
          y' = -x * sin θ + y * cos θ
          two = 2 :: Int

data Gabor3d = Gabor3d
             { α :: Double    -- this filters factor
             , λ :: Double    -- duh?
             , θ :: Double    -- rotation of the 2d mask
             , ψ :: Double    -- duh2?
             , σ :: Double    -- width of the 2d mask
             , γ :: Double    -- compression of the 2d mask
             , μ :: Double    -- compression of the 3d mask
             , ot :: Double   -- offset in t dimension
             , ox :: Double   -- offset in x dimension
             , oy :: Double   -- offset in y dimension
             , op :: Double   -- offset in p dimension
             } deriving (Show,Read,Eq,Ord)

lview :: Gabor3d -> [Double]
lview (Gabor3d α λ θ ψ σ γ μ ot ox oy op) = [α,λ,θ,ψ,σ,γ,μ,ot,ox,oy,op]

rlview :: [Double] -> Gabor3d
rlview [α,λ,θ,ψ,σ,γ,μ,ot,ox,oy,op] = (Gabor3d α λ θ ψ σ γ μ ot ox oy op)
rlview ps                          = error $ "couldn't create Gabor3d from: " ++ show ps

evalGabor :: Gabor3d -> NominalDiffTime -> Int -> Int -> DVS.Polarity -> Double
evalGabor (Gabor3d{..}) t x y p 
    = realPart $ gabor3d λ θ ψ σ γ μ (toSeconds t-ot) (fromIntegral x-ox) (fromIntegral y-oy)

--------------------------------------------------------------------------------
type Gabors = [Gabor3d]
type SpikeTrain = V.Vector (DVS.Event DVS.Address)

type Score = Double

-- create Entity instance for 3d gabor kernels

instance Entity Gabors Score SpikeTrain () IO where
    genRandom _ seed = return $ evalRand (genRandomGabors 10) (mkStdGen seed)
    crossover _ param seed a b = return $ evalRand (Just <$> crossoverGabors a b) (mkStdGen seed)
    mutation  _ param seed a = return $ evalRand (Just <$> mutationGabors a) (mkStdGen seed)
    --score     spikes gabors  = Just <$> scoreGabor spikes gabors
    scorePop spikes _ gs = scoreGabors spikes gs



-- implementations


genRandomGabor :: (Applicative m, MonadRandom m) => m Gabor3d
genRandomGabor = Gabor3d <$> getRandom
                         <*> getRandom
                         <*> getRandom
                         <*> getRandom
                         <*> getRandom
                         <*> getRandom
                         <*> getRandom
                         <*> getRandom
                         <*> getRandom
                         <*> getRandom
                         <*> getRandom

genRandomGabors n = replicateM n genRandomGabor

--crossoverGabor a b = Gabor3d <$> uniform [α a, α b]
--                             <*> uniform [λ a, λ b]
--                             <*> uniform [θ a, θ b]
--                             <*> uniform [ψ a, ψ b]
--                             <*> uniform [σ a, σ b]
--                             <*> uniform [γ a, γ b]
--                             <*> uniform [μ a, μ b]

crossoverGabors as bs = do
    n <- getRandomR (0,length as)
    return $ take n as ++ drop n bs

--mutationGabor :: (Applicative m, MonadRandom m) => Gabor3d -> m Gabor3d
mutationGabor a = do
    traceM $ "mutate gabor: " ++ show a
    rα <- getRandom
    rλ <- getRandom
    rθ <- getRandom
    rψ <- getRandom
    rσ <- getRandom
    rγ <- getRandom
    rμ <- getRandom
    rot <- getRandom
    rox <- getRandom
    roy <- getRandom
    rop <- getRandom
    ma <- Gabor3d <$> uniform [α a, α a + rα]
                  <*> uniform [λ a, λ a + rλ]
                  <*> uniform [θ a, θ a + rθ]
                  <*> uniform [ψ a, ψ a + rψ]
                  <*> uniform [σ a, σ a + rσ]
                  <*> uniform [γ a, γ a + rγ]
                  <*> uniform [μ a, μ a + rμ]
                  <*> uniform [ot a, ot a + rot]
                  <*> uniform [ox a, ox a + rox]
                  <*> uniform [oy a, oy a + roy]
                  <*> uniform [op a, op a + rop]

    uniform [a,ma]

mutationGabors = mapM mutationGabor

-----------------------------------------------------------------------------




-----------------------------------------------------------------------------


gaborsToDistribution :: Gabors -> (NominalDiffTime -> Int -> Int -> DVS.Polarity -> Double) 
gaborsToDistribution gs ts x y pol = sum [ evalGabor g ts x y pol | g <- gs ]


eventSqDistance (DVS.qview -> (ap,ax,ay,at)) (DVS.qview -> (bp,bx,by,bt)) = dt + dx + dy + dp
    where dt = (toSeconds at - toSeconds bt)^(2::Int)
          dx = (fromIntegral ax - fromIntegral bx)^(2::Int)
          dy = (fromIntegral ay - fromIntegral by)^(2::Int)
          dp = if ap == bp then 0 else 1

halfSpikeTrain :: SpikeTrain -> SpikeTrain
halfSpikeTrain spikes = V.generate (V.length spikes `quot` 2) (\i -> go (spikes V.! (i*2)) (spikes V.! ((i*2)+1)))
    where go (DVS.Event (DVS.Address ap ax ay) at)
             (DVS.Event (DVS.Address bp bx by) bt) 
                = DVS.Event (DVS.Address ap ((ax+bx)`quot`2) ((ay+by)`quot`2)) ((at+bt)/2)

scoreGabor :: SpikeTrain -> Gabors -> IO Double
scoreGabor aer gs  = do
    traceM $ "score gabor: " ++ show gs
    traceM $ "spikes count: " ++ show (V.length aer)

    let headTS = DVS.timestamp $ V.head aer
        lastTS = DVS.timestamp $ V.last aer

    -- 1. create distribution
    let distFun = gaborsToDistribution gs

    -- 2. synthesize spiketrain from gs
    synthST' <- synthesize (V.length aer) (headTS,lastTS) distFun
    traceM $ "synth length: " ++ show (length synthST')
    let synthST = V.fromListN (V.length aer) synthST'

    -- 3. use dtw to calculate error
    let c = cost $ fastDtw eventSqDistance halfSpikeTrain 2 aer synthST

    traceM $ "score: " ++ show c

    return c

scoreGabors :: SpikeTrain -> [Gabors] -> IO (Maybe [Maybe Double])
scoreGabors spikes gs = do
    traceM "scoring gabors"
    scoreGabors <- mapM (scoreGabor spikes) gs
    let parGabors = scoreGabors `using` parList rdeepseq
    return $ Just (map Just parGabors)

main :: IO ()
main = do

    server <- forkServer "localhost" 8888

    let c = GAConfig 1 10 5 0.8 0.2 0 0 True False

    g <- getStdGen
    (Right aer) <- fmap V.fromList <$> DVS.readDVSData "../aer/data/DVS128-2014-10-21T17-41-53+0200-20000-0.aedat"

    archive <- evolveVerbose g c () aer :: IO (Archive [Gabor3d] Double)

    print archive

