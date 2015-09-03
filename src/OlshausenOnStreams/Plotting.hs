


{-# LANGUAGE ViewPatterns #-}

module OlshausenOnStreams.Plotting where

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import           Data.Foldable

import           Linear



eventsToPlot es = Plot3D.cloud Graph3D.points . fmap (\(V3 x y z) -> (x,y,z)) . toList $ es 

plotEvents es = GP.plotDefault $  Frame.cons defltOpts (eventsToPlot es)
multiplotEvents es = GP.plotDefault $ Frame.cons defltOpts (mconcat $ fmap eventsToPlot $ es)

plotFile fn es = GP.plot terminal gfx
  where terminal = PNG.cons fn
        gfx      = Frame.cons (defltOpts) (eventsToPlot es)

multiplotFile fn es = GP.plot terminal gfx
  where terminal = PNG.cons fn
        gfx      = Frame.cons defltOpts (mconcat $ fmap eventsToPlot $ es)


defltOpts :: (Atom.C x, Atom.C y, Atom.C z)
          => Opts.T (Graph3D.T x y z)
defltOpts = Opts.key True
          . Opts.xLabel "x pos" 
          . Opts.yLabel "y pos" 
          . Opts.zLabel "time (s)" 
          $ Opts.deflt


