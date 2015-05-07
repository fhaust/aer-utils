

{-# LANGUAGE ParallelListComp #-}

module LearnGaborTests where





import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.HUnit.Approx

import Control.Monad

import Data.AER.EBCCSP15

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as V


testCreationOfSinit :: Assertion
testCreationOfSinit = do
    imgs <- readCSVMat "data/mats/x.csv" :: IO [Img Double]
    phis <- readCSVMat "data/mats/a.csv" :: IO [Phi Double]
    expectedAs <- readCSVVec 64 "data/mats/sinit.csv" :: IO [UV.Vector Double]

    let actualAs = map (\img -> UV.fromList $ initialAs img phis) imgs

    -- the actual tests
    zipWithM_ (V.zipWithM_ (assertApproxEqual "" 1e-12)) expectedAs actualAs

testCalculateResiduals :: Assertion
testCalculateResiduals = do
    imgs       <- readCSVMat "data/mats/x.csv" :: IO [Img Double]
    phis       <- readCSVMat "data/mats/a.csv" :: IO [Phi Double]
    as         <- readCSVVec 64 "data/mats/s.csv" :: IO [UV.Vector Double]
    expectedEs <- readCSVMat "data/mats/e.csv" :: IO [Mat Double]

    let actualEs = residualError imgs (map V.toList as) phis

    zipWithM_ (matEq' 1e-12) expectedEs actualEs

testUpdateBases :: Assertion
testUpdateBases = do
    phis       <- readCSVMat "data/mats/a.csv" :: IO [Phi Double]
    es         <- readCSVVec 64 "data/mats/e.csv" :: IO [UV.Vector Double]
    as         <- readCSVVec 64 "data/mats/s.csv" :: IO [UV.Vector Double]
    expectedDA <- readCSVMat "data/mats/da.csv" :: IO [Mat Double]

    let actualDA = updateBases (map (Mat . V.convert) es) (map V.toList as)

    zipWithM_ (matEq' 1e-12) expectedDA actualDA

    expectedPhis' <- readCSVMat "data/mats/a-next.csv" :: IO [Mat Double]

    let η = 1
        actualPhis' = [ φ + η `scale'` dφ | φ <- phis | dφ <- expectedDA ]

    zipWithM_ (matEq' 1e-12) expectedPhis' actualPhis'

-- sparsenet code line 71-74
testSVarUpdate :: Assertion
testSVarUpdate = do
    ss <- V.toList <$$> readCSVVec 64 "data/mats/s.csv" :: IO [[Double]]
    let initialSvars = replicate 64 0.1
        actualSvars = adjustAVars initialSvars ss
        expectedSvars = 
          [0.09099061618532134,0.09089253321405526,0.09127534292471856,0.09133147454290834
          ,0.09161642155645495,0.09109925186339823,0.09093698938418794,0.09253837137235758
          ,0.09074436165271649,0.09079102597028114,0.09076969908902772,0.0915947144660344
          ,0.09126803869211067,0.09082562464555125,0.09407068709167948,0.09127445393685948
          ,0.0910450292269496,0.0920507517176162,0.09320271093181684,0.09094972731469846
          ,0.09167341029501416,0.09124622359262273,0.09123823574774725,0.092574948713591
          ,0.09079417770207081,0.09105392322483927,0.09084501388054683,0.09175735797047362
          ,0.0909374032029556,0.09079500760654194,0.09090917120653831,0.09105118756912564
          ,0.09084773375775407,0.09103935142205091,0.09189168119419977,0.0907847817927069
          ,0.09229511024994548,0.09130592265436924,0.09187809476651321,0.0910258642239057
          ,0.09305754089457763,0.09078656359118877,0.09090884028611616,0.09071912933691388
          ,0.09108299666188317,0.0914916754793933,0.09267797205627065,0.09228300361952124
          ,0.09128296632975862,0.09126534280039128,0.09085499902304374,0.09158798203494999
          ,0.09166296847117356,0.09137078047043108,0.0910224602486133,0.09110449154972498
          ,0.0907270958066394,0.09075841534320653,0.09102356930286638,0.09112223757985671
          ,0.09124992287304839,0.09109938815914005,0.09080005039687465,0.09073521393027012
          ] :: [Double]

    zipWithM_ (assertApproxEqual "" 1e-12) expectedSvars actualSvars

    let alpha   = 0.02 :: Double
        gain    = replicate 64 1

        actualGain   = adjustGain alpha gain actualSvars
        expectedGain = 
          [0.9981135055990377,0.9980919759758142,0.9981758757126217,0.9981881489834975
          ,0.9982503393291574,0.9981373250559518,0.998101737111171,0.9984502660429856
          ,0.9980594084808567,0.9980696707227835,0.9980649812364565,0.9982456083811149
          ,0.9981742780862634,0.9980772761973485,0.9987782729971661,0.9981756812744653
          ,0.9981254396832461,0.9983447691693523,0.9985931230312286,0.9981045330862988
          ,0.9982627545041015,0.9981695058084238,0.9981677581053645,0.9984581575984649
          ,0.9980703636534712,0.9981273896855087,0.9980815370941871,0.9982810289978367
          ,0.9981018279504019,0.998070546109961,0.9980956296879265,0.9981267899149556
          ,0.9980821347316982,0.9981241947312173,0.9983102356437472,0.9980682978262403
          ,0.9983977045963561,0.9981825629378104,0.9983072833737727,0.9981212371363095
          ,0.9985619916289882,0.9980686895966316,0.9980955570240108,0.9980538573300053
          ,0.9981337627122918,0.9982231365700631,0.9984803684551065,0.9983950851706982
          ,0.9981775430167046,0.9981736883927061,0.9980837310395094,0.9982441408593554
          ,0.9982604802861778,0.9981967388896171,0.9981204906139187,0.9981384732056892
          ,0.9980556101298381,0.9980624996599557,0.9981207338424938,0.9981423613356161
          ,0.9981703151428635,0.9981373549226659,0.9980716547443815,0.9980573961417321
          ] :: [Double]

    zipWithM_ (assertApproxEqual "" 1e-12) expectedGain actualGain

learnGaborTests = testGroup "learn gabor tests"
    [ testCase "test sinit creation" testCreationOfSinit
    , testCase "test residual calculation" testCalculateResiduals
    , testCase "test update bases" testUpdateBases
    , testCase "test s_var update" testSVarUpdate
    ]

matEq' ε (Mat a) (Mat b) = V.zipWithM_ (assertApproxEqual "" ε) a b
