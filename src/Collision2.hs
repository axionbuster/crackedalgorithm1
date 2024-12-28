-- | "effectful" collision detection and resolution
module Collision2 where

import Collision
import Control.Monad.Fix
import Data.Kind
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Linear

-- | collision resolution data type
data Resolve a = Resolve
  { -- | position
    respos :: !(V3 a),
    -- | displacement
    resdis :: !(V3 a)
  }
  deriving (Show, Eq, Ord)

-- | get a block's shape at integer coordinates
data GetBlock (f :: Type -> Type) a :: Effect where
  -- | get a block's shape at integer coordinates
  GetBlock :: !(V3 Int) %1 -> GetBlock f a m (ManyBoxes f a)

type instance DispatchOf (GetBlock f a) = Dynamic

-- | get a block's shape at integer coordinates
getblock ::
  (HasCallStack, GetBlock f a :> ef) =>
  V3 Int -> Eff ef (ManyBoxes f a)
getblock = send . GetBlock

-- | detect and resolve collision
resolve ::
  (Reader (Box n) :> ef, GetBlock [] n :> ef) =>
  Resolve n -> Eff ef (Resolve n)
resolve = fix \continue resolution -> do
  undefined
