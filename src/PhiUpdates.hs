

module PhiUpdates where


import           Numeric.GSL.Minimization
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as S
import qualified Data.Vector as V
import           Types
import           Common
import           Linear

updatePhis :: Patch Double -> Phis Double -> As Double -> Phis Double
updatePhis patch phis fittedAs = fst $ updatePhis' patch phis fittedAs

updatePhis' :: Patch Double -> Phis Double -> As Double -> (Phis Double, LA.Matrix Double)
updatePhis' patch phis fittedAs = (unflattenV numPhis result, mat)
  where (result, mat) = minimizeV NMSimplex2 precision iterations searchBox errorFun (flattenTimestamps phis)
        precision  = 1e-9 -- TODO decide on parameters for this
        iterations = 1000 
        {-searchBox  = S.replicate (V.sum (V.map S.length phis) * 3) 1 -- whatever-}
        searchBox  = S.replicate (V.sum (V.map S.length phis)) 1 -- whatever
        numPhis    = V.length phis
        errorFun :: S.Vector Double -> Double
        errorFun vs = reconstructionError patch (unsafeUnflattenTimestamps phis vs) fittedAs

flatten :: Phi Double -> S.Vector Double
flatten = S.unsafeCast 

unflatten :: S.Vector Double -> Phi Double
unflatten = S.unsafeCast

flattenV :: Phis Double -> S.Vector Double
flattenV = S.concat . V.toList . V.map flatten


unflattenV :: Int -> S.Vector Double -> Phis Double
unflattenV n vs | remainder /= 0  = error $ "invalid size for unflattening, n = " ++ show n ++ ", length vs = " 
                                                                                  ++ show (S.length vs)
                | otherwise = V.generate n (\i -> S.unsafeSlice (i*len) len vs')
    where vs' = unflatten vs
          (len,remainder) = S.length vs' `divMod` n


flattenTimestamps :: Phis Double -> S.Vector Double
flattenTimestamps = S.concat . V.toList . V.map (S.map (\(V3 _ _ t) -> t))

-- | this is mostly unsafe because there are no checks for length
-- | should be fine as long as it is just used here
unsafeUnflattenTimestamps :: Phis Double -> S.Vector Double -> Phis Double
unsafeUnflattenTimestamps phis ts | V.sum (V.map S.length phis) /= S.length ts = error "sizes don't match"
                                  | otherwise = V.unfoldr go (phis,ts)
  where go (phis,ts) | V.null phis = Nothing
                     | otherwise   = Just (merged, (V.unsafeTail phis, S.unsafeDrop (S.length h) ts))
                        where
                          merged = S.zipWith (\(V3 x y _) t -> V3 x y t) h ts
                          h      = V.unsafeHead phis


joinTimestamps :: Phis Double -> Phis Double -> Phis Double
joinTimestamps = V.zipWith (S.zipWith (\(V3 x y _) (V3 _ _ t) -> V3 x y t))
