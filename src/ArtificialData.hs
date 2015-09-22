
module ArtificialData where

import           Linear
import           Control.Monad
import           Control.Monad.Random

randomPlane ::
  (Floating a, Ord a, Show a, Epsilon a, Random a, MonadRandom m) =>
  Int -> m [V3 a]
randomPlane num = do
    x <- getRandomR (0, 1)
    y <- getRandomR (0, 1)
    t <- getRandomR (0, 1)
    let o = V3 2.5 2.5 2.5
        n = normalize $ V3 x y t

    plane o n num

plane :: 
  (Floating a, Ord a, Show a, Epsilon a, Random a, MonadRandom m) =>
  V3 a -> V3 a -> Int -> m [V3 a]
plane o n num = do
    let u = normalize $ perpendicular n
        v = cross n u

    {-traceM $ "o = " ++ show o-}
    {-traceM $ "n = " ++ show n-}
    {-traceM $ "u = " ++ show u-}
    {-traceM $ "v = " ++ show v-}


    replicateM num $ do
      fu <- getRandomR (-2.5,2.5)
      fv <- getRandomR (-2.5,2.5)
      return $ o + (fu *^ u) + (fv *^ v)


perpendicular :: (Num a, Ord a) => V3 a -> V3 a
perpendicular v@(V3 x y z) | m == x    = V3 0 (-z) y
                           | m == y    = V3 (-z) 0 x
                           | otherwise = V3 (-y) x 0
    where m = minimum v
