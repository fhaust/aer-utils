


module Main where



import           Graphics.Gloss
import           FRP.Ordrea

import qualified Data.Matrix     as M

import qualified Data.AER.AEDat  as AER
import qualified Data.AER.DVS    as AER

main :: IO ()
main = do


    (Right aer) <- AER.readDVSData "../common/chessboard.aedat"


    playDVSData aer


    print "done"



playDVSData aer = play (InWindow "AER Data" (800,600) (0,0))
                       (greyN 0.5)
                       60
                       aer
                       

