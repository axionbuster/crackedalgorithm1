-- | "effectful" collision detection and resolution
module Collision2 where

import Collision
import Control.Lens hiding (index)
import Control.Monad
import Control.Monad.Fix
import Data.Kind
import Data.List (sortBy)
import Data.Ord
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
      -- compute the times ("hits") at which the object will hit a block
      -- and then sort in ascending order of time
      (sortBy (comparing hitprop) -> hits) <-
        concat <$> for fps \fp -> do
          -- shoot ray & break at first hit
          let raystart = scenter myself + (fromIntegral <$> fp)
           in march raystart disp0 & fix \continuerm rm -> case rm of
                -- this should be impossible
                [] -> pure []
                -- no hit
                (t, _, _) : rm' | t > 1 -> continuerm rm'
                -- grid points, are there any blocks?
                (t, _, gridpoints) : rm' ->
                  gridpoints & fix \continuegp gp -> case gp of
                    -- no more grid points, so no
                    [] -> continuerm rm'
                    -- let's check the block at the grid point
                    gp' : gp'' -> do
                      let consbelow =
                            -- go below and check too
                            getblock (gp' - V3 0 1 0) >>= \case
                              Just blockbelow
                                | Just hitbelow <-
                                    hitting disp0 myself blockbelow ->
                                    pure (hitbelow :)
                              _ -> pure id
                          cond ? action = if cond then action else pure id
                          blockshort b = shicorner b ^. _y < 0.5
                      -- check if the block at the grid point exists & is solid
                      -- also just in case a tall block (like a fence)
                      -- is there, we check the block below it
                      getblock gp' >>= \case
                        Just block
                          -- note: ray location and myself location
                          -- are independent of each other
                          | Just hit <- hitting disp0 myself block ->
                              -- oh, we hit something
                              (blockshort block ? consbelow) <*> pure [hit]
                          | otherwise ->
                              -- a block is there but we don't hit it
                              (blockshort block ? consbelow) <*> continuegp gp''
                        -- no block at the grid point
                        Nothing -> consbelow <*> continuegp gp''
      error "do something with hits" hits
