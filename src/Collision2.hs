-- | "effectful" collision detection and resolution
module Collision2 where

import Collision
import Control.Lens hiding (index)
import Control.Monad
import Control.Monad.Fix
import Data.Kind
import Data.Traversable
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import Face
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
  GetBlock :: !(V3 Int) %1 -> GetBlock f a m (f a)

type instance DispatchOf (GetBlock f a) = Dynamic

-- | get a block's shape at integer coordinates
getblock ::
  (HasCallStack, GetBlock f a :> ef) =>
  V3 Int -> Eff ef (f a)
getblock = send . GetBlock

-- | detect and resolve collision
resolve ::
  (Shape s, RealFloat n, Epsilon n, Reader (Box n) :> ef, GetBlock s n :> ef) =>
  -- | shape of the object who is moving
  s n ->
  -- | attempted displacement
  V3 n ->
  -- | collision detection and resolution
  Eff ef (Resolve n)
resolve myself disp0 =
  Resolve (scenter myself) disp0
    & fix \continue resolution -> do
      -- sample some points off the relevant faces of the object
      -- we will grid march along the rays (of the displacement) shot
      -- from these points
      let fps = facepoints a b
            where
              V2 a b = fmap (fmap ceiling) $ corners myself
      for fps \fp ->
        do undefined
      undefined
