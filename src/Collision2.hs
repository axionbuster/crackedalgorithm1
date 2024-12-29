-- | "effectful" collision detection and resolution
module Collision2
  ( GetBlock (..),
    Resolve (..),
    NewlyTouchingGround (..),
    getblock,
    resolve,
  )
where

import Collision
import Control.Lens hiding (index)
import Control.Monad.Fix
import Data.Functor.Rep
import Data.Kind
import Data.List (minimumBy)
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
    resdis :: !(V3 a),
    -- | did it newly touch ground?
    restou :: !NewlyTouchingGround
  }
  deriving (Show, Eq)

-- | newly touching ground?
--
-- - If 'True', the object is newly touching the ground.
-- - If 'False' and the object was touching the ground, it is no longer.
-- - If 'False' and the object was not touching the ground, it still isn't.
newtype NewlyTouchingGround = NewlyTouchingGround {newonground :: Bool}
  deriving newtype (Show, Eq, Ord, Enum, Bounded)

-- | get a block's shape at integer coordinates (dynamic effect)
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
{-# INLINE getblock #-}

-- | detect and resolve collision
resolve ::
  (Shape s, RealFloat n, Epsilon n, GetBlock s n :> ef) =>
  -- | shape of the object who is moving
  s n ->
  -- | attempted displacement
  V3 n ->
  -- | new resolution
  --
  -- unless it got stuck, the new displacement should be near zero
  Eff ef (Resolve n)
resolve myself disp =
  Resolve (scenter myself) disp (NewlyTouchingGround False)
    & if nearZero disp then pure else resolve' myself
{-# INLINE resolve #-}

-- the actual implementation of 'resolve'
resolve' ::
  forall s n ef.
  (Shape s, RealFloat n, Epsilon n, GetBlock s n :> ef) =>
  s n ->
  Resolve n ->
  Eff ef (Resolve n)
resolve' =
  -- this is a loop that will run until the displacement is resolved
  -- (i.e., until it stops moving due to resolution or being blocked)
  fix \continue myself resolution -> do
    -- sample some points off the relevant faces of the object
    -- we will grid march along the rays (of the displacement) shot
    -- from these points
    let disp = resdis resolution
        fps = facepoints a b
          where
            V2 a b = fmap ceiling <$> corners myself
        minimum_ [] = Nothing
        minimum_ xs = Just $ minimumBy (comparing hittime) xs
    -- compute the times ("hits") at which the object will hit a block
    -- and then find the earliest hit
    mearliest <-
      minimum_ . concat <$> for fps \fp ->
        -- shoot ray & break at first hit
        let raystart = scenter myself + (fromIntegral <$> fp)
         in march raystart disp & fix \continuerm rm -> case rm of
              -- this should be impossible
              [] -> pure []
              -- no hit
              (t, _, _) : rm' | t > 1 -> continuerm rm'
              -- grid cubes, are there any blocks?
              (_, _, cubes) : rm' ->
                cubes & fix \continuecb cb -> case cb of
                  -- no more grid points, so no
                  [] -> continuerm rm'
                  -- let's check the block at the grid point
                  cb' : cb'' -> do
                    let consbelow =
                          -- go below and check too
                          getblock (cb' - V3 0 1 0) <&> \case
                            Just blockbelow
                              | Just hitbelow <-
                                  hitting disp myself blockbelow ->
                                  (hitbelow :)
                            _ -> id
                        True ? action = action
                        False ? _ = pure id
                        short b = shicorner b ^. _y < 0.5
                    -- check if the block at the grid point exists & is solid
                    -- also just in case a tall block (like a fence)
                    -- is there, we check the block below it
                    getblock cb' >>= \case
                      Just block
                        -- note: ray location and myself location
                        -- are independent of each other
                        | Just hit <- hitting disp myself block ->
                            -- oh, we hit something
                            (short block ? consbelow) <*> pure [hit]
                        | otherwise ->
                            -- a block is there but we don't hit it
                            (short block ? consbelow) <*> continuecb cb''
                      -- no block at the grid point
                      Nothing -> consbelow <*> continuecb cb''
    case mearliest of
      Nothing ->
        -- no collision
        pure resolution
      Just earliest -> do
        -- now correct the displacement; advance position
        let (!) = index
            flush x | nearZero x = 0
            flush x = x
            delta = flush <$> (hittime earliest *^ disp)
            collided = (/= 0) <$> hitnorm earliest
            resdis = tabulate \i ->
              if collided ! i
                then 0 -- collision cancels out the displacement
                else (1 - hittime earliest) * (disp ! i)
            respos = scenter myself + delta
            restou = NewlyTouchingGround $ hitnorm earliest ^. _y > 0
        Resolve {resdis, respos, restou}
          & if or collided
            && not (and collided)
            && not (nearZero resdis)
            then
              continue $ translate respos myself
            else
              pure
