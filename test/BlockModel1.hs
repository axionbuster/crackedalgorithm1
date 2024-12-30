-- | block module for testing
module BlockModel1 (Model (..), runBlockModel) where

import Collision
import Collision2
import Data.Map.Strict (Map, lookup)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Linear
import Prelude hiding (lookup)

-- | block model
newtype Model = Model (Map (V3 Int) (Box Double))
  deriving stock (Show, Eq)

-- | run 'GetBlock' effect
runBlockModel :: Model -> Eff (GetBlock f n : ef) a -> Eff ef a
runBlockModel (Model m) = reinterpret (evalState m) \_ -> \case
  GetBlock i -> gets (lookup i)
