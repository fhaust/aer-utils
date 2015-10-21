


module Minimization where



import           IntegralBased
import           Numeric.LinearAlgebra
import           Linear

import           Graphics.Gnuplot.Simple

import qualified Data.Vector as V


ltxBasePath = "/home/florian/Dokumente/Studium/NE/master/thesis/gfx/"
commonAttributes = [XLabel "iteration", YLabel "coefficient", Size (Scale 0.5)]

-- | test gradient descent of two overlapping spikes
-- expected result: a stays at 1
test1 = do
    let patch = fromList [V3 0 0 0] :: Patch Double
        phis  = V.fromList [fromList [V3 0 0 0]] :: Phis Double

    let initialAs = fromList [5]
        fittedAs  = gradientDescentToFindAs' patch phis initialAs

    -- plot result
    let curve = toList . head . drop 3 . toColumns . snd $ fittedAs
        fn    = ltxBasePath ++ "minimization-two-overlapping-spikes"
    epspdfPlot fn (\as -> plotList (as ++ commonAttributes) curve)

-- | test gradient descent of two non overlapping spikes
test2 = do
    let patch = fromList [V3 0 0 0] :: Patch Double
        phis  = V.fromList [fromList [V3 10 0 0]] :: Phis Double

    let initialAs = fromList [5]
        fittedAs  = gradientDescentToFindAs' patch phis initialAs

    -- plot result
    let curve = toList . head . drop 3 . toColumns . snd $ fittedAs
        fn    = ltxBasePath ++ "minimization-two-non-overlapping-spikes"
    epspdfPlot fn (\as -> plotList (as ++ commonAttributes) curve)


-- | test gradient descent of three half overlapping spikes
test3 = do
    let patch = fromList [V3 0 0 0, V3 10 0 0] :: Patch Double
        phis  = V.fromList [fromList [V3 10 0 0]] :: Phis Double

    let initialAs = fromList [5]
        fittedAs  = gradientDescentToFindAs' patch phis initialAs

    -- plot result
    let curve = toList . head . drop 3 . toColumns . snd $ fittedAs
        fn    = ltxBasePath ++ "minimization-three-half-overlapping-spikes"
    epspdfPlot fn (\as -> plotList (as ++ commonAttributes) curve)

-- | test gradient descent of three half overlapping spikes
test4 = do
    let patch = fromList [V3 1 0 0] :: Patch Double
        phis  = V.fromList [fromList [V3 0 0 0, V3 2 0 0]] :: Phis Double

    let initialAs = fromList [5]
        fittedAs  = gradientDescentToFindAs' patch phis initialAs

    -- plot result
    let curve = toList . head . drop 3 . toColumns . snd $ fittedAs
        fn    = ltxBasePath ++ "minimization-three-half-fitting-spikes"
    epspdfPlot fn (\as -> plotList (as ++ commonAttributes) curve)
