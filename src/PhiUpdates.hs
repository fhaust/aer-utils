

module PhiUpdates where


import           Numeric.GSL.Minimization
import qualified Numeric.LinearAlgebra as LA
import qualified Data.Vector.Storable as S
import qualified Data.Vector as V
import           VanRossumError
import           Types
import           Linear
import           Common

{-updatePhisForPatches :: Int -> Patches Double -> Phis Double -> V.Vector (As Double)-}
{-                     -> (Phis Double, LA.Matrix Double)-}
{-updatePhisForPatches iterations patches phis fittedAs = (unflattenV numPhis result, mat)-}
{-  where (result, mat) = minimizeV NMSimplex2 precision iterations searchBox errorFun (flattenV phis)-}
{-        precision  = 1e-6 -- TODO decide on parameters for this-}
{-        [>iterations = 100  -- just do one step in the right direction<]-}
{-        searchBox  = S.replicate ((V.sum $ V.map S.length phis)*3) 0.1 -- whatever-}
{-        numPhis    = V.length phis-}
{-        errorFun :: S.Vector Double -> Double-}
{-        errorFun vs = V.sum $ V.zipWith (\patch as -> reconstructionError patch (unflattenV numPhis vs) as) patches fittedAs-}

updatePhis :: Patch Double -> Phis Double -> As Double -> Phis Double
updatePhis patch phis fittedAs = fst $ updatePhis' patch phis fittedAs

updatePhis' :: Patch Double -> Phis Double -> As Double -> (Phis Double, LA.Matrix Double)
updatePhis' patch phis fittedAs = (unflattenV numPhis result, mat)
  where (result, mat) = minimizeV NMSimplex2 precision iterations searchBox errorFun (flattenV phis)
        precision  = 1e-9 -- TODO decide on parameters for this
        iterations = 1000 
        searchBox  = S.replicate ((V.sum $ V.map S.length phis)*3) 1 -- whatever
        numPhis    = V.length phis
        errorFun :: S.Vector Double -> Double
        errorFun vs = reconstructionError patch (unflattenV numPhis vs) fittedAs

flatten :: Phi Double -> S.Vector Double
flatten = S.unsafeCast 

unflatten :: S.Vector Double -> Phi Double
unflatten = S.unsafeCast

flattenV :: Phis Double -> S.Vector Double
flattenV = S.concat . V.toList . V.map flatten

unflattenV :: Int -> S.Vector Double -> Phis Double
unflattenV n vs | rem /= 0  = error $ "invalid size for unflattening, n = " ++ show n ++ ", length vs = " 
                                                                            ++ show (S.length vs)
                | otherwise = V.generate n (\i -> S.unsafeSlice (i*len) len vs')
    where vs' = unflatten vs
          (len,rem) = S.length vs' `divMod` n



