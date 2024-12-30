-- | block module for testing
module BlockModel1 (Model (..), runBlockModel) where

import Collision2
import Data.Map.Strict (Map, lookup)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Linear
import Prelude hiding (lookup)

-- | block model with the container type @f and numeric type @a
--
-- you could use, say Box or ManyBoxes for @f and Double for @a
newtype Model f a = Model (Map (V3 Int) (f a))
  deriving stock (Show, Eq)

-- | run 'GetBlock' effect
runBlockModel :: Model f n -> Eff (GetBlock f n : ef) a -> Eff ef a
runBlockModel (Model m) = reinterpret (evalState m) \_ -> \case
  GetBlock i -> gets (lookup i)