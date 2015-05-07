


module Main where



import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck

import qualified Data.Sequence as S

import Data.Functor


import LearnGaborTests




main :: IO ()
main = defaultMain 
     [ learnGaborTests
     ]
