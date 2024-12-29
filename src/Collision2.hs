-- | "effectful" collision detection and resolution
module Collision2 where

import Collision
import Control.Lens hiding (index)
import Control.Monad
import Control.Monad.Fix
import Data.Kind
import Data.Maybe (catMaybes)
import Data.Traversable
import Effectful
import Effectful.Dispatch.Dynamic
import Face
import Linear
import March

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
  GetBlock :: !(V3 Int) %1 -> GetBlock f a m (Maybe (f a))

type instance DispatchOf (GetBlock f a) = Dynamic

-- | get a block's shape at integer coordinates
getblock ::
  (HasCallStack, GetBlock f a :> ef) =>
  -- | integer coordinates
  V3 Int ->
  -- | if (relevant) block exists, return its shape
  --
  -- what block is \"relevant\" is up to the implementation
  Eff ef (Maybe (f a))
getblock = send . GetBlock

-- | detect and resolve collision
resolve ::
  forall s n ef.
  (Shape s, RealFloat n, Epsilon n, GetBlock s n :> ef) =>
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
              V2 a b = fmap ceiling <$> corners myself
      -- compute the times at which the object will hit the grid
      -- or positive infinity if it doesn't hit anything
      hits <-
        catMaybes <$> for fps \fp -> do
          -- shoot ray & break at first hit
          let raystart = scenter myself + (fromIntegral <$> fp)
           in march raystart disp0 & fix \continue2 rm -> case rm of
                -- this should be impossible
                [] -> pure $ Nothing
                -- no hit
                (t, _, _) : rm' | t > 1 -> continue2 rm'
                -- grid points, are there any blocks?
                (t, _, gridpoints) : rm' -> do
                  gridpoints & fix \continue3 gp -> case gp of
                    -- no more grid points, so no
                    [] -> continue2 rm'
                    -- let's check the block at the grid point
                    gp' : gp'' -> do
                      -- check if the block at the grid point exists & is solid
                      getblock gp' >>= \case
                        Just block
                          -- note: ray location and myself location
                          -- are independent of each other
                          | Just hit <- hitting disp0 myself block ->
                              -- oh, we hit something
                              pure $ Just hit
                          | otherwise ->
                              -- a block is there but we don't hit it
                              continue3 gp''
                        -- no block at the grid point
                        Nothing -> continue3 gp''
      error "do something with hits" hits
