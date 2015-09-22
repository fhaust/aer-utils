


{-# LANGUAGE ViewPatterns #-}

module OlshausenOnStreams.Plotting where

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import           Data.Foldable

import           Linear

import           GHC.IO.Exception

import qualified Data.Vector.Storable as S


eventsToPlot ::
  (Foldable t, Tuple.C a, Atom.C a) => t (V3 a) -> Plot3D.T a a a
eventsToPlot es = Plot3D.cloud Graph3D.points . fmap (\(V3 x y z) -> (x,y,z)) . toList $ es

plotEvents es = multiplotEvents [es]
multiplotEvents es = GP.plotDefault $ Frame.cons defltOpts (mconcat $ map eventsToPlot es)

multiplotEventsAsync es = GP.plotAsync (X11.cons) $ Frame.cons defltOpts (mconcat $ map eventsToPlot es)
multiplotEventsAsyncS es = multiplotEventsAsync (toList . fmap S.toList $ es)

plotFile fn es = multiplotFile fn [es]

multiplotFile fn es = GP.plot terminal gfx
  where terminal = PNG.cons fn
        gfx      = Frame.cons defltOpts (mconcat $ fmap eventsToPlot $ es)

multiplotFileS fn es = multiplotFile fn (toList . fmap S.toList $ es)


defltOpts :: (Num x, Atom.C x, Tuple.C x
             ,Num y, Atom.C y, Tuple.C y
             ,Fractional z, Atom.C z, Tuple.C z)
          => Opts.T (Graph3D.T x y z)
defltOpts = Opts.key False
          . Opts.xLabel "x pos" 
          . Opts.yLabel "y pos" 
          . Opts.zLabel "time (s)" 
          . Opts.xRange3d (0, 5)
          . Opts.yRange3d (0, 5)
          . Opts.zRange3d (0, 5)
          $ Opts.deflt


