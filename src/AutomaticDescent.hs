
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module AutomaticDescent where


import           Control.Monad.Zip
import           Numeric.AD
import           Numeric.AD.Mode
import           Numeric.AD.Jacobian
import           Numeric.AD.Internal.Identity
import           Numeric.AD.Internal.Forward.Double
import           Numeric.AD.Internal.Forward
import           Data.Foldable
import           Data.List.Split
import           GHC.TypeLits
import           Foreign.Storable
--import           ForwardVector

import qualified Data.Vector as B
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed.Deriving

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R


import           GHC.Exts
import           GHC.Types


-----------------------------------------------------------------------------

type Img r a = R.Array R.U R.DIM2 a
type Phi r a = R.Array R.U R.DIM2 a
{-type Patch a = M.Matrix a-}
{-type As a  = [a]-}

{-type Mats a = B.Vector (M.Matrix a)-}
{-type Imgs a = B.Vector (Img a)-}
{-type Phis a = [Phi a]-}
{-type Patches a = B.Vector (Patch a)-}
{-type Ass      a = B.Vector (As      a)-}

-----------------------------------------------------------------------------

reconstruct :: (Num a, Foldable t, MonadZip t, R.Shape sh, R.Source r a) 
            => t a -> t (R.Array r sh a) -> R.Array R.D sh a
reconstruct as φs = foldl1 (R.zipWith (+)) [ R.map (*a) φ | a <- as | φ <- φs ]

sparseness :: Floating a => a -> a -> a
sparseness σ a = s (a / σ) where s x = log (1+x*x)

{-preserveInfos :: (Foldable t, MonadZip t, U.Unbox a, Num a)-}
{-              => Array D-}
preserveInfos patch φs as = R.sumAllS (R.map (^(2::Int)) (R.zipWith (-) patch (reconstruct as φs)))

{-errorFunction :: (Foldable t, MonadZip t, Floating a) -}
{-              => a -> a -> a -> Patch a -> t (Phi a) -> t a -> a-}
{-errorFunction :: (Floating a, Num (M.Vector a), M.Container M.Vector a)-}
{-              => a -> a -> a -> M.Matrix a -> [M.Matrix a] -> [a] -> a-}
errorFunction λ β σ patch φs as = i + s
  where i = λ * preserveInfos patch φs as 
        s = bos * sum [ sparseness σ a | a <- as ]
        bos = β / σ

--errorFunction' :: () 
--               => ForwardDouble -> ForwardDouble -> ForwardDouble
--               -> R.Array R.U R.DIM2 ForwardDouble
--               -> [R.Array R.U R.DIM2 ForwardDouble]
--               -> [ForwardDouble]
--               -> ForwardDouble
--errorFunction' = errorFunction


--findAsForImg :: forall a. ()
--             => ForwardDouble -> ForwardDouble -> ForwardDouble
--             -> R.Array R.U R.DIM2 ForwardDouble
--             -> [R.Array R.U R.DIM2 ForwardDouble]
--             -> [ForwardDouble]
--             -> [[ForwardDouble]]
--findAsForImg :: forall r a. (Floating a, R.Source r a)
--             => a -> a -> a -> R.Array r R.DIM2 a
--             -> [R.Array r R.DIM2 a]
--             -> [a]
--             -> [[a]]
--findAsForImg λ β σ patch φs = conjugateGradientDescent go
--  where  go :: forall t. (Scalar t ~ a, Mode t, Floating t, R.Elt t, U.Unbox t)
--            => [t] -> t
--         go = errorFunction (auto λ) (auto β) (auto σ) (R.map auto patch) (R.map auto <$> φs)

--findAsForImg :: forall r a. (Floating a, R.Source r a)
--             => a -> a -> a -> R.Array r R.DIM2 a
--             -> [R.Array r R.DIM2 a]
--             -> [a]
--             -> [[a]]
--findAsForImg λ β σ patch φs = gradientDescent go
--  where  go :: forall t. (Scalar t ~ a, Mode t, Floating t, R.Elt t, U.Unbox t)
--            => [t] -> t
--         go = errorFunction (auto λ) (auto β) (auto σ) (R.map auto patch) (R.map auto <$> φs)

{-findAsForImg' :: (Floating a, Ord a)-}
{-              => a -> a -> a -> Patch a -> B.Vector (Phi a) -> As a -> As a-}
{-findAsForImg' λ β σ patch φs = go . findAsForImg λ β σ patch φs-}
{-  where go (a:b:xs) | errDiff a b < 0.01 = b-}
{-                    | otherwise          = go (b:xs)-}
{-        errFun = errorFunction λ β σ patch φs-}
{-        errDiff a b = abs (errFun a - errFun b)-}

{-initialAs :: Fractional a => Patch a -> B.Vector (Phi a) -> As a-}
{-initialAs patch = V.map go-}
{-  where go φ = sumElems' (φ * patch) / norm-}
{-          where norm = sumElems' $ φ^(2::Int)-}

{-initialAs patches phis = B.fromList [ B.zipWith (/) s normA2 | s <- sinit ]-}
{-    where sinit  = map B.fromList . chunksOf 64 . toList -}
{-                 $ [ M.sumElements (patch * phi) | patch <- patches,  phi <- phis ]-}
{-          normA2 = [ M.sumElements (phi*phi) | phi <- phis ]-}



-------------------------


instance R.Elt ForwardDouble where
    {-# INLINE touch #-}
    touch (ForwardDouble (D# p) (D# t)) = IO (\s0 -> case touch# p s0 of s1 -> case touch# t s1 of s2 -> (# s2, () #))
      -- IO (\state -> case touch# d state of state' -> (# state', () #))

    {-# INLINE zero #-}
    zero = 0

    {-# INLINE one #-}
    one = 1


-------------------------

{-instance MonadZip B.Vector where-}
{-    mzip = B.zip-}
{-    mzipWith = B.zipWith-}
{-    munzip = B.unzip-}

{---------------------------}


derivingUnbox "ForwardVector"
    [t| ForwardDouble -> (Double, Double) |]
    [| \ (ForwardDouble a b) -> (a,b) |]
    [| \ (a, b) -> (ForwardDouble a b) |]

--zipWithRecycle f xs ys
--    | m < n = G.generate n $ \i -> f (xs G.! mod i m) (ys G.! i)
--    | otherwise = G.generate m $ \i -> f (xs G.! i) (ys G.! mod i n)
--  where (m, n) = (G.length xs, G.length ys)

type Mat2d a = R.Array R.U R.DIM2 a

instance Num (Mat2d ForwardDouble) where
  fromInteger 0  = zero
  fromInteger n = auto (fromInteger n)
  (+)          = binary (+) 1 1
  (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
  (*)          = lift2 (*) (\x y -> (y, x))
  negate       = lift1 negate (const (auto (-1)))
  abs          = lift1 abs signum
  signum a     = lift1 signum (const zero) a

rAll f = R.foldAllS (&&) True . R.map f
rSingleton n = R.fromList (R.Z R.:. 1 R.:. 1) [n]

instance Mode (Mat2d ForwardDouble) where
    type Scalar (Mat2d ForwardDouble) = Double
    auto = rSingleton . auto
    zero = rSingleton zero
    isKnownZero = rAll isKnownZero
    isKnownConstant = rAll isKnownConstant 
    a *^ xs = R.computeS $ R.map (a*^) xs
    xs ^* a = R.computeS $ R.map (^* a) xs
    xs ^/ a = R.computeS $ R.map (^/ a) xs

instance Jacobian (Mat2d ForwardDouble) where
    type D (Mat2d ForwardDouble) = Id Double
    unary f a b = R.computeS $ R.map (unary f a) b
    lift1 f df  = R.computeS . R.map (lift1 f df)
    lift1_ f df = R.computeS . R.map (lift1_ f df)
    binary f da db a b = R.computeS $ R.zipWith (binary f da db) a b
    lift2 f df a b = R.computeS $ R.zipWith (lift2 f df) a b
    lift2_ f df a b = R.computeS $ R.zipWith (lift2_ f df) a b

{-instance Storable ForwardDouble where-}
{-    sizeOf    _ = 2*8-}
{-    alignment _ = 8-}
{-    peekByteOff addr off = ForwardDouble <$> peekByteOff addr off <*> peekByteOff addr (off+8)-}
{-    pokeByteOff addr off (ForwardDouble p t) = pokeByteOff addr off p >> pokeByteOff addr (off+8) t-}

{-instance M.Element ForwardDouble-}

{-[>instance M.Container M.Vector ForwardDouble where<]-}
{-[>    sumElements' m = G.sum m<]-}


{---------------------------}


{-infixl 4 <$$>-}
{-(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f0 (f1 a) -> f0 (f1 b)-}
{-f <$$> x = fmap (fmap f) x-}
{-[># INLINE (<$$>) #<]-}
