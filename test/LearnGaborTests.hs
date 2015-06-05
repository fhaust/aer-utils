

{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DataKinds #-}

module LearnGaborTests where





import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Approx

import Control.Monad
import Control.Monad.State

import LearnGabors

import Data.Foldable

import Data.SimpleMat

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as V
import qualified Data.Vector         as B


getPhis :: IO (Phis Double)
getPhis = readCSVMats "data/mats/a.csv"

getPatches :: IO (Imgs 8 8 Double)
getPatches = readCSVMats "data/mats/x.csv"

getInitialAs :: IO (Ass Double)
getInitialAs = readCSVVecs "data/mats/sinit.csv"

getFittedAs :: IO (Ass Double)
getFittedAs = readCSVVecs "data/mats/s.csv"

getResidual :: IO (Mats 8 8 Double)
getResidual = readCSVMats "data/mats/e.csv"

getDA :: IO (Mats 8 8 Double)
getDA = readCSVMats "data/mats/da.csv" 

getNextPhis :: IO (Mats 8 8 Double)
getNextPhis = readCSVMats "data/mats/a-next.csv" 

-- cgf_fitS.m:30
testCreationOfSinit :: Assertion
testCreationOfSinit = do
    patches <- getPatches
    phis    <- getPhis
    expectedAs <- getInitialAs

    let actualAs = fmap (\patch -> initialAs patch phis) patches

    -- the actual tests
    V.zipWithM_ (V.zipWithM_ (assertApproxEqual "" 1e-12)) expectedAs actualAs

testCalculateResiduals :: Assertion
testCalculateResiduals = do
    patches    <- getPatches
    phis       <- getPhis
    as         <- getFittedAs
    expectedEs <- getResidual

    let actualEs = residualError patches as phis

    V.zipWithM_ (matEq' 1e-12) expectedEs actualEs

testUpdateBases :: Assertion
testUpdateBases = do
    phis       <- getPhis
    es         <- getResidual
    as         <- getFittedAs
    expectedDA <- getDA

    let actualDA = updateBases es as

    V.zipWithM_ (matEq' 1e-12) expectedDA actualDA

    expectedPhis' <- getNextPhis

    let η = 1
    let actualPhis' = V.zipWith (\φ dφ ->  φ + η `scale` dφ) phis expectedDA

    V.zipWithM_ (matEq' 1e-12) expectedPhis' actualPhis'

-- sparsenet code line 71-74
testAdjustPhiVariance :: Assertion
testAdjustPhiVariance = do

    ass <- readCSVVecs "data/mats/test-adjust-phi-var-fitted-as.csv" :: IO (Ass Double)
    phis <- readCSVMats "data/mats/test-adjust-phi-var-phis.csv" :: IO (Phis Double)
    initialPhis <- getPhis -- only needed to initialize that state

    let state = mkInitialIS initialPhis

    let state' = execState (adjustPhisForVariance alpha ass phis) state

    let actualPhis = isPhis state'
        actualVars = isVars state'
        actualGain = isGain state'
  
    expectedGain <- readCSVVec "data/mats/test-adjust-phi-var-next-gain.csv"
    expectedVars <- readCSVVec "data/mats/test-adjust-phi-var-next-vars.csv"
    expectedPhis <- readCSVMats "data/mats/test-adjust-phi-var-next-phis.csv" :: IO (Phis Double)

    V.zipWithM_ (assertApproxEqual "" 1e-12) expectedGain actualGain
    V.zipWithM_ (assertApproxEqual "" 1e-12) expectedVars actualVars
    V.zipWithM_ (matEq' 1e-12) expectedPhis actualPhis


learnGaborTests = testGroup "learn gabor tests"
    [ testCase "test sinit creation" testCreationOfSinit
    , testCase "test residual calculation" testCalculateResiduals
    , testCase "test update bases" testUpdateBases
    , testCase "test adjust phis for variance" testAdjustPhiVariance
    ]

matEq' ε a b = zipWithM_ (assertApproxEqual "" ε) (toList a) (toList b)
