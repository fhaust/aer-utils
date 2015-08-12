
{-# LANGUAGE ParallelListComp #-}

module OlshausenOnStreams.ArtificialData where




import qualified Data.AER.DVS128 as DVS


movingEdge p dt = concat [ edge p x t | t <- [0,dt..] | x <- [0..127] ]



edge p x t = [ DVS.Event (DVS.Address p x y) t | y <- [0..127] ]
