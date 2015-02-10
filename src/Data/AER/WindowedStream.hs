


module Data.AER.WindowedStream where


import qualified Data.AER.Types as AER

import           Data.Thyme.Clock

import           Data.List
import           Data.List.Split

windowedStream :: NominalDiffTime -> [AER.Event a] -> [[AER.Event a]]
windowedStream dt xs = go t0 xs
    where t0 = AER.timestamp (head xs)
          go _  []  = []
          go tn xs' = now : go (tn+dt) later
              where (now,later) = partition (\e -> AER.timestamp e < tn + dt) xs'



