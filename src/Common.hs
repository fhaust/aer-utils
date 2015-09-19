


module Common where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S

import Types
import VanRossumError
import Linear

-- | reconstruction error
reconstructionError :: Patch Double -> Phis Double -> As Double -> Double
reconstructionError patch phis as = errorIntegral (phis' S.++ patches')
  where phis'    = V.foldl' (S.++) S.empty $ V.zipWith (\a -> S.map (\(V3 x y z) -> V4 a x y z)) (V.convert as) phis
        patches' = S.map (\(V3 x y z) -> V4 (-1) x y z) patch
