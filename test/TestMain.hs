


module Main where



import Data.AER.Similarity

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck

import qualified Data.Sequence as S

import Data.Functor


newtype SmallNonEmptySeq a = SmallNonEmptySeq { getSmallNonEmpty :: [a] }
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (SmallNonEmptySeq a) where
    arbitrary = SmallNonEmptySeq <$> listOf arbitrary `suchThat` (\l -> length l > 2 && length l < 10)

sqDist :: Double -> Double -> Double
sqDist x y = abs (x-y)

{-testDTWVSDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool-}
{-testDTWVSDTWNaive (la,lb) = abs (dtwNaive sqDist sa sb - dtw sqDist sa sb) < 0.01-}
{-  where sa = S.fromList $ getSmallNonEmpty la-}
{-        sb = S.fromList $ getSmallNonEmpty lb-}

testDTWMemoVSDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool
testDTWMemoVSDTWNaive (la,lb) = abs (dtwNaive sqDist sa sb - cost (dtwMemo sqDist sa sb)) < 0.01
  where sa = S.fromList $ getSmallNonEmpty la
        sb = S.fromList $ getSmallNonEmpty lb

testFastDTWvsDTWNaive :: (SmallNonEmptySeq Double, SmallNonEmptySeq Double) -> Bool
testFastDTWvsDTWNaive (la,lb) = abs (dtwNaive sqDist sa sb - cost (fastDtw sqDist 10 sa sb)) < 0.01
  where sa = S.fromList $ getSmallNonEmpty la
        sb = S.fromList $ getSmallNonEmpty lb

main :: IO ()
main = defaultMain 
     {-[ testProperty "dtw == dtwNaive" testDTWVSDTWNaive-}
     [ testProperty "dtwMemo == dtwNaive" testDTWMemoVSDTWNaive
     , testProperty "fastDtw == dtwNaive" testFastDTWvsDTWNaive
     ]
