

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module IntegralBased  where

import           Data.Number.Erf
import           Data.List

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
{-import qualified Data.Vector.Unboxed as U-}
import           Linear


import           Control.Monad.Random

import           Debug.Trace

import           Orphans

import           Numeric.GSL.Integration
import           Numeric.GSL.Minimization


type Event a  = V3 a
type Events a = V.Vector (Event a)

type Patch a = Events a
type Phi a   = Events a
type As a    = V.Vector a

type Patches a = V.Vector (Patch a)
type Phis a    = V.Vector (Phi a)


gradientDescentToFindAs :: Patch Double -> Phis Double -> As Double -> S.Vector Double
gradientDescentToFindAs patch phis randomAs = fst $ minimizeV NMSimplex2 10e-9 1000 (S.replicate (length phis) 1) (\v -> stDistance patch phis (V.convert v)) (V.convert randomAs)


{-gradientDescentToUpdatePhis patch phis fittedAs =-}

-- | distance between several spike trains
stDistance :: Patch Double -> Phis Double -> As Double -> Double
stDistance patch phis coeffs = realIntegral' (V.toList (phis' V.++ patches')) (V3 0 0 (-10)) (V3 128 128 10)
    where 
          phis'    = foldl (V.++) V.empty  $ V.zipWith (\a phi -> V.map (\(V3 x y z) -> V4 a x y z) phi) coeffs phis
          patches' = V.map (\(V3 x y z) -> V4 (-1) x y z) patch



-- not needed, is integrated in SAGE code
{-gauss a b c x = a * exp ( Linear.dot (negate (x-b) *! c) (x-b))-}



test = do


    patch <- V.replicateM 16 $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> pure 0.5) :: IO (Patch Double)
    phis  <- V.replicateM 4 $ V.replicateM 16 
                            $ (V3 <$> getRandomR (0,128) <*> getRandomR (0,128) <*> getRandomR (0,1)) :: IO (Phis Double)

    randomAs <- V.replicateM (length phis) getRandom :: IO (As Double)


    {-let fittedAs = gradientDescentToFindAs patch phis randomAs-}

    {-putStrLn $ show (fittedAs !! 0)-}

    return (patch, phis, randomAs)



infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap (fmap f) x

infixl 4 <$$$>
(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
f <$$$> x = fmap (fmap (fmap f)) x




{-twoGaussIntegralR (V3 lx ly lz) (V3 hx hy hz) a b  = twoGaussIntegral a b hx hy hz - twoGaussIntegral a b lx ly lz-}

{--- | copied from sage-}
{-twoGaussIntegral (V4 aa ax ay az) (V4 ba bx by bz) x y z = -}
{-    1/4*pi**(3/2)*aa*ba*erf(-1/2*ax - 1/2*bx + x)*erf(-1/2*ay - 1/2*by +-}
{-    y)*erf(-1/2*az - 1/2*bz + z)*exp(-1/4*ax**2 - 1/4*ay**2 + 1/4*(az + bz)**2 --}
{-    1/2*az**2 + 1/2*ax*bx - 1/4*bx**2 + 1/2*ay*by - 1/4*by**2 - 1/2*bz**2) +-}
{-    1/8*pi**(3/2)*aa**2*erf(-ax + x)*erf(-ay + y)*erf(-az + z) +-}
{-    1/8*pi**(3/2)*ba**2*erf(-bx + x)*erf(-by + y)*erf(-bz + z)-}


oneIteration :: Patches Double -> Phis Double -> Phis Double
oneIteration patches phis = V.zipWith (V.zipWith (+)) phis pushVs

    where pushVs = collapsePushVectors
                 . V.map (\patch -> oneIterationPatch patch phis) 
                 $ patches

pushVector :: Num a => (V3 a -> V3 a) -> Phi a -> a -> V.Vector (V3 a)
pushVector dPatch phi fittedA = V.map (\e -> fittedA *^ dPatch e) phi

pushVectors :: Num a => (V3 a -> V3 a) -> Phis a -> As a -> V.Vector (V.Vector (V3 a))
pushVectors dPatch = V.zipWith (pushVector dPatch)

collapsePushVectors :: Fractional a => V.Vector (V.Vector (V.Vector (V3 a))) -> V.Vector (V.Vector (V3 a))
collapsePushVectors vs = V.foldl1' (V.zipWith (V.zipWith (\a b -> a + (b/n)))) vs
    where n = fromIntegral $ length vs

oneIterationPatch :: Patch Double -> Phis Double -> V.Vector (V.Vector (V3 Double))
oneIterationPatch patch phis = pushVectors dPatch phis (V.convert fittedAs)
    where -- find best as
          fittedAs = gradientDescentToFindAs (V.convert patch) phis (V.replicate (V.length phis) 1)
          -- prepare gradient field
          dPatch = realDerivates (V.toList . V.map (\(V3 x y z) -> V4 (-1) x y z) $ patch)


gauss :: V4 Double -> Double
gauss (V4 a b c d) = a * exp( -0.5 * (b**2+c**2+d**2) )
errFun :: [V4 Double] -> V3 Double -> Double
errFun gs (V3 x y z) = (sum [ gauss (V4 a (x-b) (y-c) (z-d)) | (V4 a b c d) <- gs ])**2
intFun :: [V4 Double] -> (Double,Double)
intFun gs = integrateQAGI 1e-9 1000 (\z -> fst $ integrateQAGI 1e-9 1000 (\y -> fst $ integrateQAGI 1e-9 1000 (\x -> errFun gs (V3 x y z)) ))

{-realIntegral :: [V4 Double] -> Double-}
{-realIntegral vs = (2*pi)**(3/2) * sum [ a**2 | (V4 a _ _ _) <- vs ]-}
{-                +  2*pi**(3/2)  * sum [ sum [ ai * aj * g (bi-bj) (ci-cj) (di-dj) | (V4 aj bj cj dj) <- is] | ((V4 ai bi ci di):is) <- tails vs ]-}
{-    where g a b c = exp ( -0.25 * (a**2+b**2+c**2))-}

realIntegral' vs (V3 lx ly lz) (V3 hx hy hz) = indefIntegral hx hy hz  - indefIntegral hx hy lz  - indefIntegral hx ly hz  + indefIntegral hx ly lz  
                                             - indefIntegral lx hy hz  + indefIntegral lx hy lz  + indefIntegral lx ly hz  - indefIntegral lx ly lz 
    where indefIntegral x y z = realIntegral vs (V3 x y z)

realIntegral vs (V3 x y z) = foo + bar
    where foo = 1/8*pi**(3/2) * sum [ a**2 * erf(x - b) * erf(y - c) * erf(z - d) | (V4 a b c d) <- vs ]
          bar = 1/4*pi**(3/2) * sum [ sum [ aj*ai * erf(x - (bj+bi)/2) * erf(y - (cj+ci)/2) * erf(z - (dj+di)/2) * exp( - 1/4*(bi-bj)**2 - 1/4*(ci-cj)**2  - 1/4*(di-dj)**2) | (V4 aj bj cj dj) <- is ] | ((V4 ai bi ci di):is) <- tails vs ]


realDerivateX vs (V3 x y z) = -2 * foo * bar
  where foo = sum [ a * (x - b) * gauss (V4 a (x-b) (y-c) (z-d))  | (V4 a b c d) <- vs ]
        bar = sum [ gauss (V4 a (x-b) (y-c) (z-d)) | (V4 a b c d) <- vs ]
  
realDerivateY vs (V3 x y z) = -2 * foo * bar
  where foo = sum [ a * (y - c) * gauss (V4 a (x-b) (y-c) (z-d))  | (V4 a b c d) <- vs ]
        bar = sum [ gauss (V4 a (x-b) (y-c) (z-d)) | (V4 a b c d) <- vs ]

realDerivateZ vs (V3 x y z) = -2 * foo * bar
  where foo = sum [ a * (z - d) * gauss (V4 a (x-b) (y-c) (z-d))  | (V4 a b c d) <- vs ]
        bar = sum [ gauss (V4 a (x-b) (y-c) (z-d)) | (V4 a b c d) <- vs ]

realDerivates vs v = V3 (realDerivateX vs v) (realDerivateY vs v) (realDerivateZ vs v)


{-realIntegralDiffX vs (V3 x y z) = foo + bar-}
{-  where foo = (pi/4) * sum [ a**2 * exp(-(c-y)^2) * erf(d - z) | (V4 a b c d) <- vs ]-}
{-        bar = (pi/2) * sum [ sum [ ai*aj * exp(-(1/4)*(bi+bj-2*x)**2) * erf ((1/2)*(ci+cj-2*y)) * erf ((1/2)*(di+dj-2*z)) * exp ((1/4)*( -(bi-bj)**2 - (ci-cj)**2 - (di-dj)**2))  | (V4 aj bj cj dj) <- is ] | ((V4 ai bi ci di):is) <- tails vs ]-}

realIntegralDiffX vs (V3 x y z) = foo + bar
  where foo = (pi/4) * sum [ a**2 * erf(y - c) * erf(z - d) * exp(-x**2 + 2*x*b - b**2) | (V4 a b c d) <- vs ]
        bar = (pi/2) * sum [ ai * sum [ aj*erf(y - 1/2*ci - 1/2*cj)*erf(z - 1/2*di - 1/2*dj) * exp(-x**2 + x*bi - 1/2*bi**2 + x*bj - 1/2*bj**2 - 1/4*ci**2 + 1/2*ci*cj - 1/4*cj**2 - 1/4*di**2 + 1/2*di*dj - 1/4*dj**2) | (V4 aj bj cj dj) <- is ] | ((V4 ai bi ci di):is) <- tails vs ]
