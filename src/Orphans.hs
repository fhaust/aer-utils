
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Control.Monad.Random
import Linear

instance Random a => Random (V2 a) where
    randomR (V2 lx ly, V2 hx hy) 
      = runRand (V2 <$> getRandomR (lx,hx) <*> getRandomR (ly,hy))
    random = runRand (V2 <$> getRandom <*> getRandom)

instance Random a => Random (V3 a) where
    randomR (V3 lx ly lz, V3 hx hy hz) 
      = runRand (V3 <$> getRandomR (lx,hx) <*> getRandomR (ly,hy) <*> getRandomR (lz,hz))
    random = runRand (V3 <$> getRandom <*> getRandom <*> getRandom)

instance Random a => Random (V4 a) where
    randomR (V4 lx ly lz lw, V4 hx hy hz hw) 
      = runRand (V4 <$> getRandomR (lx,hx) <*> getRandomR (ly,hy) <*> getRandomR (lz,hz) <*> getRandomR (lw,hw))
    random = runRand (V4 <$> getRandom <*> getRandom <*> getRandom <*> getRandom)
