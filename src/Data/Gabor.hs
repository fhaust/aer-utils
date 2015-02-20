

module Data.Gabor where


import Data.Complex

import System.Random

--import Codec.Picture

import Data.Word

-- | this is the gabor function
gabor :: RealFloat a 
      => a  -- ^ wavelength of underlying sines
      -> a  -- ^ orientation of underlying sines
      -> a  -- ^ offset/shift of underlying sines
      -> a  -- ^ size/radius of the gauss bell
      -> a  -- ^ compression/ratio of the gauss bell
      -> a  -- ^ x - position
      -> a  -- ^ y - position
      -> Complex a
gabor λ θ ψ σ γ x y = exp ( (-0.5) * ((x'^2 + γ^2*y'^2) / (σ^2)) :+ 0) * exp ( 0 :+ (2*pi*(x'/λ+ψ)) )
    where x' =  x * cos θ + y * sin θ
          y' = -x * sin θ + y * cos θ

{-gabor3d λ θ ψ σ γ st x y t = exp ( (-0.5) * ((t^2) / (st^2)) :+ 0 ) * exp ( (-0.5) * ((x'^2 + γ^2*y'^2) / (σ^2)) :+ 0) * exp ( 0 :+ (2*pi*(x'/λ+ψ)) )-}
{-    where x' =  x * cos θ + y * sin θ-}
{-          y' = -x * sin θ + y * cos θ-}

-- | wrapper for storing gabor parameters
data Gabor a = Gabor 
  { λ  :: !a
  , θ  :: !a
  , ψ  :: !a
  , σ  :: !a
  , γ  :: !a
  , ox :: !a
  , oy :: !a
  } deriving (Show,Read,Ord,Eq)

mkGabor λ θ ψ σ γ ox oy               = Gabor λ θ ψ σ γ ox oy 
evalGabor (Gabor λ θ ψ σ γ ox oy) x y = gabor λ θ ψ σ γ (x-ox) (y-oy)

{--- http://en.wikipedia.org/wiki/Gaussian_function-}
{-gauss σ x = exp ( (-0.5) * (x^2 / σ^2) )-}




