
{-# LANGUAGE DeriveFunctor #-}

module OlshausenOnStreams.Types where


import qualified Data.Vector as V

data Event a = Event {
    time :: {-# UNPACK #-} !a,
    posX :: {-# UNPACK #-} !a,
    posY :: {-# UNPACK #-} !a,
    val  :: {-# UNPACK #-} !a 
} deriving (Show,Read,Eq,Functor)

instance Num a => Num (Event a) where
    a + b = Event (time a + time b) (posX a + posX b) (posY a + posY b) (val a + val b)
    a - b = Event (time a - time b) (posX a - posX b) (posY a - posY b) (val a - val b)
    a * b = Event (time a * time b) (posX a * posX b) (posY a * posY b) (val a * val b)
    abs a = Event (abs (time a)) (abs (posX a)) (abs (posY a)) (abs (val a))
    signum a = Event (signum (time a)) (signum (posX a)) (signum (posY a)) (signum (val a))
    fromInteger a = Event (fromInteger a) (fromInteger a) (fromInteger a) (fromInteger a)

instance Fractional a => Fractional (Event a) where
    a / b   = Event (time a / time b) (posX a / posX b) (posY a / posY b) (val a / val b)
    fromRational a = Event (fromRational a) (fromRational a) (fromRational a) (fromRational a)

instance Floating a => Floating (Event a) where
    sqrt (Event t x y p) = Event (sqrt t) (sqrt x) (sqrt y) (sqrt p)

scaleEvent :: Num a => a -> Event a -> Event a
scaleEvent f e = Event 
  { time = f * time e
  , posX = f * posX e
  , posY = f * posY e
  , val  = f * val  e
  }

{-U.derivingUnbox "Event"-}
{-    [t| Event -> (NominalDiffTime, Float, Float, Float) |]-}
{-    [| \ (Event t x y p) -> (t,x,y,p) |]-}
{-    [| \ (t,x,y,p) -> (Event t x y p) |]-}

type Events a = V.Vector (Event a)
type Patch  a = Events a
type Phi    a = Events a
