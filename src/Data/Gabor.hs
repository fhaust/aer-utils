

module Data.Gabor where


import Data.Complex

import System.Random

--import Codec.Picture

import Data.Word

-- | this is the gabor function
gabor λ θ ψ σ γ x y = exp ( (-0.5) * ((x'^2 + γ^2*y'^2) / (σ^2)) :+ 0) * exp ( 0 :+ (2*pi*(x'/λ+ψ)) )
    where x' =  x * cos θ + y * sin θ
          y' = -x * sin θ + y * cos θ

gabor3d λ θ ψ σ γ st x y t = exp ( (-0.5) * ((t^2) / (st^2)) :+ 0 ) * exp ( (-0.5) * ((x'^2 + γ^2*y'^2) / (σ^2)) :+ 0) * exp ( 0 :+ (2*pi*(x'/λ+ψ)) )
    where x' =  x * cos θ + y * sin θ
          y' = -x * sin θ + y * cos θ


-- http://en.wikipedia.org/wiki/Gaussian_function
gauss σ x = exp ( (-0.5) * (x^2 / σ^2) )




randomGabor g0 = (gabor λ θ ψ σ γ, g5)
    where (λ,g1) = randomR (-1,1)   g0
          (θ,g2) = randomR (-pi,pi) g1
          (ψ,g3) = randomR (-1,1)   g2
          (σ,g4) = randomR (-1,1)   g3
          (γ,g5) = randomR (-1,1)   g4

randomGabors g0 = gabor : randomGabors g1
    where (gabor,g1) = randomGabor g0


imgSize :: Int
imgSize = 512

--gaborPixel :: Double -> Int -> Int -> Word8
--gaborPixel t x y = floor . (*127) . (+1) . realPart $ gabor3d 1 0 1 1 1 1 x' y' t
--    where x' = ((fromIntegral x / fromIntegral imgSize) - 0.5) * 6
--          y' = ((fromIntegral y / fromIntegral imgSize) - 0.5) * 6

{-gaborImg t size = generateImage pixelFun size size-}
{-    where pixelFun x y = fromIntegral $ (*127) . (+1) $ realPart $ gabor3d 1 0 1 1 1 1 x' y' t-}
{-              where x' = ((fromIntegral x-halfSize)/(5*halfSize))-}
{-                    y' = ((fromIntegral y-halfSize)/(5*halfSize))-}
{-          halfSize = fromIntegral size / 2-}


