


{-# LANGUAGE ViewPatterns #-}

module OlshausenOnStreams.AERPlot where

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Value.Atom as Atom

import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import qualified Data.AER.DVS128 as DVS

import           Data.Foldable

import           OlshausenOnStreams.Types

wave3d :: Frame.T (Graph3D.T Double Double Double)
wave3d =
   let meshNodes = linearScale 20 (-2,2)
   in  Frame.cons
          (Opts.grid True $
           Opts.view 42 17 1 1 $
           Opts.xRange3d (-2.5,2.5) $
           Opts.yRange3d (-2.5,2.5) $
           defltOpts) $
       Plot3D.surface
          meshNodes meshNodes
          (\x y -> cos(x*x+y*y))

lissajous3d :: Frame.T (Graph3D.T Double Double Double)
lissajous3d =
   let t = linearScale 300 (-pi, pi)
       f n = map (sin . (*) n) t
   in  Frame.cons defltOpts $
       Plot3D.cloud Graph3D.impulses $
       zip3 (f 3) (f 4) (f 9)


spikeToPos (DVS.qview -> (_,x,y,t)) = (fromIntegral x, fromIntegral y, realToFrac t)

eventsToPlot es = Plot3D.cloud Graph3D.points . map (\(Event t x y v) -> (x,y,t)) . toList $ es 

plotEvents es = GP.plotDefault $  Frame.cons defltOpts (eventsToPlot es)
multiplotEvents es = GP.plotDefault $ Frame.cons defltOpts (mconcat $ fmap eventsToPlot $ es)

defltOpts :: (Atom.C x, Atom.C y, Atom.C z)
          => Opts.T (Graph3D.T x y z)
defltOpts = Opts.key False 
          . Opts.xLabel "x pos" 
          . Opts.yLabel "y pos" 
          . Opts.zLabel "time (s)" 
          $ Opts.deflt


