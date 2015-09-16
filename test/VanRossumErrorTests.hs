

module VanRossumErrorTests where

import           VanRossumError
import           Numeric.LinearAlgebra
import           Numeric.GSL.Integration
import           Linear

import           Graphics.Gnuplot.Simple


import           Test.QuickCheck
import           TestOrphans

testIntegralError :: (NonEmptyList (V4 Double)) -> Bool
testIntegralError (NonEmpty vs) = abs (symbolic - numeric) <= delta
  where symbolic = errorIntegral svs
        (numeric,delta) = numericErrorIntegral svs
        svs = fromList vs


---

ltxBasePath = "/home/florian/Dokumente/Studium/NE/master/thesis/gfx/"
commonAttributes = [XLabel "spike position", YLabel "error", Size (Scale 0.5)]

-- show the behavior of two spikes in two trains, where one is moving
-- "over" the other
test1 = do
    let spike1   = V4 1 0 0 0
        spike2 x = V4 (-1) x 0 0 
        spikes x = fromList [spike1, spike2 x]

    let error x = errorIntegral (spikes x)

    {-plotPath [Key (Just ["spike spike error"])] errors-}
    let range   = linearScale 1000 (-10,10)
        fn      = ltxBasePath ++ "two-spike-error"
    {-plotFunc attribs range error-}
    s <- inclPlot fn (\as -> plotFunc (as ++ commonAttributes) range error)
    putStrLn s

-- show the behavior of three spikes in two trains, where the single spike
-- is moving in the x range

test2 = do
    let spikes1 x = fromList [V4 1 x 0 0]
        spikes2 = fromList [V4 (-1) (-5) 0 0, V4 (-1) 5 0 0]
        spikes x = vjoin [spikes1 x, spikes2]

    let error x = errorIntegral (spikes x)

    {-plotPath [Key (Just ["spike spike error"])] errors-}
    let range   = linearScale 1000 (-15,15)
        fn      = ltxBasePath ++ "three-spike-error"
    {-plotFunc attribs range error-}
    s <- inclPlot fn (\as -> plotFunc (as ++ commonAttributes) range error)
    putStrLn s


-- | two spikes at the same position, one of them is scaled on a
test3 = do
    let spikes1 x = fromList [V4 x 0 0 0]
        spikes2 = fromList [V4 (-1) 0 0 0]
        spikes x = vjoin [spikes1 x, spikes2]

    let error x = errorIntegral (spikes x)

    {-plotPath [Key (Just ["spike spike error"])] errors-}
    let range   = linearScale 1000 (0,1)
        fn      = ltxBasePath ++ "two-spike-alpha-error"
    {-plotFunc attribs range error-}
    s <- inclPlot fn (\as -> plotFunc (as ++ commonAttributes) range error)
    putStrLn s


test4 = do
    let spikes1 x = fromList [V4 1 x 0 0, V4 1 0 0 0]
        spikes2 = fromList [V4 (-1) 0 0 0]
        spikes x = vjoin [spikes1 x, spikes2]

    let error x = errorIntegral (spikes x)

    {-plotPath [Key (Just ["spike spike error"])] errors-}
    let range   = linearScale 1000 (-10,10)
        fn      = ltxBasePath ++ "three-spike-same-position-error"
    {-plotFunc attribs range error-}
    s <- inclPlot fn (\as -> plotFunc (as ++ commonAttributes) range error)
    putStrLn s


test5 = do
    let spikes1 x = fromList [V4 1 x 0 0, V4 1 0 0 0]
        spikes2 = fromList [V4 (-1) 0 0 0, V4 (-1) 5 0 0]
        spikes x = vjoin [spikes1 x, spikes2]

    let error x = errorIntegral (spikes x)

    {-plotPath [Key (Just ["spike spike error"])] errors-}
    let range   = linearScale 1000 (0,10)
        fn      = ltxBasePath ++ "three-spike-same-position-error"
    plotFunc commonAttributes range error
    {-s <- inclPlot fn (\as -> plotFunc (as ++ commonAttributes) range error)-}
    {-putStrLn s-}
