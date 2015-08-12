
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module FowardMat where


import qualified Data.Vector.Unboxed as U

import           ForwardVector

import           Numeric.AD.Internal.Forward.Double
import           Numeric.AD.Mode
import           Numeric.AD.Internal.Identity
import           Numeric.AD.Jacobian
import           Numeric.AD.Mode

import           Data.Proxy
import           GHC.TypeLits

newtype Mat (w :: Nat) (h :: Nat) where
  Mat :: U.Vector ForwardDouble -> Mat w h

width :: forall w h. KnownNat w => Mat w h -> Int
width _ = fromInteger $ natVal (Proxy :: Proxy w)

height :: forall w h. KnownNat h => Mat w h -> Int
height _ = fromInteger $ natVal (Proxy :: Proxy h)

index :: KnownNat w => Mat w h -> Int -> Int -> ForwardDouble
index m@(Mat v) x y = v U.! (x + y * width m)

instance Functor (Mat w h) where
    fmap f (Mat v) = Mat $ U.map f v
deriving instance (U.Unbox a, Show a) => Show (Mat w h a)


deriving instance Num (Mat w h ForwardDouble)

instance Mode (Mat w h ForwardDouble) where
    type Scalar (Mat w h ForwardDouble) = Double
    auto = Mat . U.singleton . auto

instance Jacobian (Mat w h ForwardDouble) where
    type D (Mat w h ForwardDouble) = Id Double
