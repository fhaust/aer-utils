
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module ForwardVector where

import Numeric.AD.Internal.Forward.Double
import Numeric.AD.Internal.On
import Numeric.AD.Mode
import Numeric.AD.Internal.Identity
import Numeric.AD.Jacobian
import Numeric.AD.Mode
import Control.Monad

import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Stream.Size as Size

derivingUnbox "ForwardVector"
    [t| ForwardDouble -> (Double, Double) |]
    [| \ (ForwardDouble a b) -> (a,b) |]
    [| \ (a, b) -> (ForwardDouble a b) |]



diff :: (ForwardDouble -> Vector ForwardDouble) -> Double -> Vector Double
diff f x = G.map tangent (f (bundle x 1))

{- |

>>> diff fun 2
fromList [1.0,4.0,12.0,32.0]

-}
fun x = U.fromList [x, x^2, x^3, x^4]

zipWithRecycle f xs ys
    | m < n = G.generate n $ \i -> f (xs G.! mod i m) (ys G.! i)
    | otherwise = G.generate m $ \i -> f (xs G.! i) (ys G.! mod i n)
  where (m, n) = (G.length xs, G.length ys)


instance Mode (Vector ForwardDouble) where
    type Scalar (Vector ForwardDouble) = Double
    auto = G.singleton . auto
    zero = G.singleton zero
    isKnownZero = G.all isKnownZero
    isKnownConstant = G.all isKnownConstant 
    a *^ xs = G.map (a*^) xs
    xs ^* a = G.map (^* a) xs
    xs ^/ a = G.map (^/ a) xs

instance Jacobian (Vector ForwardDouble) where
    type D (Vector ForwardDouble) = Id Double
    unary f a b = G.map (unary f a) b
    lift1 f df = G.map (lift1 f df)
    lift1_ f df = G.map (lift1_ f df)
    binary f da db = zipWithRecycle (binary f da db)
    lift2 f df = zipWithRecycle (lift2 f df)
    lift2_ f df = zipWithRecycle (lift2_ f df)

instance Num (Vector ForwardDouble) where
  fromInteger 0  = zero
  fromInteger n = auto (fromInteger n)
  (+)          = binary (+) 1 1
  (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
  (*)          = lift2 (*) (\x y -> (y, x))
  negate       = lift1 negate (const (auto (-1)))
  abs          = lift1 abs signum
  signum a     = lift1 signum (const zero) a


instance Fractional (Vector ForwardDouble) where
  fromRational 0 = zero
  fromRational r = auto (fromRational r)
  x / y        = x * recip y
  recip        = lift1_ recip (const . negate . join (*))

instance Floating (Vector ForwardDouble) where
  pi       = auto pi
  exp      = lift1_ exp const
  log      = lift1 log recip
  logBase x y = log y / log x
  sqrt     = lift1_ sqrt (\z _ -> recip (auto 2 * z))
  (**)     = zipWithRecycle (**)
  sin      = lift1 sin cos
  cos      = lift1 cos $ negate . sin
  tan      = lift1 tan $ recip . join (*) . cos
  asin     = lift1 asin $ \x -> recip (sqrt (auto 1 - join (*) x))
  acos     = lift1 acos $ \x -> negate (recip (sqrt (1 - join (*) x)))
  atan     = lift1 atan $ \x -> recip (1 + join (*) x)
  sinh     = lift1 sinh cosh
  cosh     = lift1 cosh sinh
  tanh     = lift1 tanh $ recip . join (*) . cosh
  asinh    = lift1 asinh $ \x -> recip (sqrt (1 + join (*) x))
  acosh    = lift1 acosh $ \x -> recip (sqrt (join (*) x - 1))
  atanh    = lift1 atanh $ \x -> recip (1 - join (*) x)
