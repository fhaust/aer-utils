
module TestOrphans where



import           Test.QuickCheck
import           Linear


instance Arbitrary a => Arbitrary (V4 a) where
    arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
