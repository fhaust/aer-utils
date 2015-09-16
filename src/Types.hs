
module Types where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import           Linear

type Event a  = V3 a
type Events a = S.Vector (Event a)

type Patch a = Events a
type Phi a   = Events a
type PushV a = Events a

type As a    = S.Vector a

type Patches a = V.Vector (Patch a)
type Phis a    = V.Vector (Phi a)
type PushVs a  = V.Vector (PushV a)
