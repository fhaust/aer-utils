

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Data.AER.Similarity where


import           Data.Sequence (Seq, ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.Sequence as S

import qualified Data.Set as Set
import qualified Data.List as L

import           Data.MemoTrie
import           Data.Function

import           Data.AdditiveGroup
import           Data.Thyme.Clock
import           Data.Thyme.Time

import qualified Data.DTW as DTW

import qualified Data.AER.DVS128 as DVS
import qualified Data.AER.Types as AER


{-aerDtw :: Floating a => [DVS.Event] -> [DVS.Event] -> DTW.Result a-}
{-aerDtw (S.fromList -> as) (S.fromList -> bs) = DTW.fastDtw evDistance 5 (fmap toEv as) (fmap toEv bs)-}


{-newtype Ev = Ev { unEv :: (Double,Double,Double,Double)}-}
{-    deriving (Show, Eq)-}

{-toEv :: DVS.Event -> Ev-}
{-toEv (AER.Event (DVS.Address p x y) t) = -}
{-    Ev (if p then 1 else (-1), fromIntegral x, fromIntegral y, fromIntegral $ toMicroseconds t)-}

{-instance Num Ev where-}
{-    (Ev (a,b,c,d)) + (Ev (e,f,g,h)) = Ev (a+e,b+f,c+g,d+h)-}
{-    (Ev (a,b,c,d)) * (Ev (e,f,g,h)) = Ev (a*e,b*f,c*g,d*h)-}
{-    (Ev (a,b,c,d)) - (Ev (e,f,g,h)) = Ev (a-e,b-f,c-g,d-h)-}
{-    negate (Ev (a,b,c,d))           = Ev (negate a, negate b, negate c, negate d)-}
{-    abs (Ev (a,b,c,d))              = Ev (abs a, abs b, abs c, abs d)-}
{-    signum (Ev (a,b,c,d))           = Ev (signum a, signum b, signum c, signum d)-}
{-    fromInteger x                   = let i = fromInteger x in Ev (i,i,i,i)-}

{-instance Fractional Ev where-}
{-    (Ev (a,b,c,d)) / (Ev (e,f,g,h)) = Ev (a/e,b/f,c/g,d/h)-}
{-    recip (Ev (a,b,c,d))            = Ev (recip a, recip b, recip c, recip d)-}

{-evDistance :: (Fractional a) => Ev -> Ev -> a-}
{-evDistance (Ev (a,b,c,d)) (Ev (e,f,g,h)) = (a-e)^^2 + (b-f)^^2 + (c-g)^^2 + (d-h)^^2-}





