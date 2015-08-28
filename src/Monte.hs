

module Monte where

import Linear
import Control.Monad.Random
import Data.List

import Orphans

import Debug.Trace

-- | compare: http://mathworld.wolfram.com/MonteCarloIntegration.html
nIntegrate3dMC ::
  (Floating t, Ord t, Random t, MonadRandom m) =>
  (V3 t -> t) -> V4 t -> V4 t -> m [(t, t)]
nIntegrate3dMC f (V4 lx ly lz lw) (V4 hx hy hz hw) = do
    let h (V4 x y z w) = if w < f (V3 x y z) then 1 else 0

    rs <- getRandomRs (V4 lx ly lz lw, V4 hx hy hz hw)

    let fs = scanl' (\(mf,mf2) x -> (mf + h x, mf2 + (h x)^(2::Int))) (0,0) rs

        area   = abs(hx-lx) * abs(hy-ly) * abs(hz-lz) * abs(hw-lw)

        calcInt n (mf,_)   = area * (mf / n)
        calcVar n (mf,mf2) = area * sqrt ( (mf2/n - (mf/n)^(2::Int)) / n)

        ms = zipWith (\n xs -> (calcInt (fromIntegral n) xs, calcVar (fromIntegral n) xs)) [1::Int,2..] fs

    return ms

{-# INLINABLE nIntegrate3dMC #-}

nIntegrate3dMCN ::
  (Floating t, Ord t, Random t, MonadRandom m) =>
  Int -> (V3 t -> t) -> V4 t -> V4 t -> m (t, t)
nIntegrate3dMCN n fun lv hv = (!! n) <$> nIntegrate3dMC fun lv hv

recursiveStratifiedSampling :: (MonadRandom m, Floating a, Ord a, Random a, Show a)
                            => a -> Int -> (V3 a -> a) -> V4 a -> V4 a -> m (a,a)
recursiveStratifiedSampling maxV n fun = go
  where go lv@(V4 lx ly lz lw) hv@(V4 hx hy hz hw) = do

          (int, var) <- (!! n) <$> nIntegrate3dMC fun lv hv

          traceM $ "lv: " ++ show lv ++ " hv: " ++ show hv ++ " int: " ++ show int ++ " +/- " ++ show var

          if var > maxV 
            then do

              let x' = (hx-lx) / 2
                  y' = (hy-ly) / 2
                  z' = (hz-lz) / 2
                  w' = (hw-lw) / 2

              xyz <- sequence [ go (V4 x y z w) (V4 (x+x') (y+y') (z+z') (w+w')) | x <- [lx,lx+x']
                                                                                 , y <- [ly,ly+y']
                                                                                 , z <- [lz,lz+z']
                                                                                 , w <- [lw,lw+w']]
              return $ foldl' (\(a,b) (c,d) -> (a+c,b+d)) (0,0) xyz

            else 
              return (int,var)
