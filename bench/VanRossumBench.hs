


module Main where


import           Criterion.Main

import           VanRossumError

import qualified Data.Vector.Storable as S
import           Linear
import           Control.Monad.Random

randomEvents :: MonadRandom m => Int -> m (S.Vector (V4 Double))
randomEvents n = S.replicateM n $ V4 <$> getRandomR (0,1)
                                     <*> getRandomR (0,128)
                                     <*> getRandomR (0,128)
                                     <*> getRandomR (0,128)

benchVanRossumErrors es =
    bgroup ("Van Rossum Error (n = " ++ show (S.length es) ++ ")")
      [-- bench "numerical integral" $ nf numericErrorIntegral es
       bench "analytical integral" $ nf errorIntegral es
      ]

benchNumericVanRossumErrors es =
    bgroup ("Numeric Van Rossum Error (n = " ++ show (S.length es) ++ ")")
      [-- bench "numerical integral" $ nf numericErrorIntegral es
       bench "numerical integral" $ nf numericErrorIntegral es
      ]


main = do
    -- seed RNG
    setStdGen (mkStdGen 0)
    -- generate random events
    es  <- randomEvents 128
    let es0 = S.force $ S.take 4 es
    let es1 = S.force $ S.take 8 es
    let es2 = S.force $ S.take 16 es
    let es3 = S.force $ S.take 32 es
    let es4 = S.force $ S.take 64 es
    let es5 = S.force $ S.take 128 es

    defaultMain 
      [ benchVanRossumErrors es0
      , benchNumericVanRossumErrors es0
      , benchVanRossumErrors es1
      , benchNumericVanRossumErrors es1
      , benchVanRossumErrors es2
      , benchNumericVanRossumErrors es2
      , benchVanRossumErrors es3
      , benchNumericVanRossumErrors es3
      , benchVanRossumErrors es4
      , benchVanRossumErrors es5
      ]
