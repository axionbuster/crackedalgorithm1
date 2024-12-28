-- | march along a line segment, finding all intersections with grid points
module March (march) where

import Control.Lens hiding (index)
import Control.Monad.Fix
import Control.Monad.ST.Lazy
import Data.Foldable
import Data.Functor
import Data.Functor.Rep
import Data.STRef.Lazy
import Linear
import Prelude hiding (read)

-- | march along a line segment, finding all intersections
-- with grid points
--
-- the starting point is NOT included in the output
--
-- a compensated sum is used to reduce floating point error
--
-- the returned list being infinite, it is recommended to
-- use 'take' to limit the number of points to be computed
march ::
  forall f a.
  ( Applicative f,
    Foldable f,
    Representable f,
    Rep f ~ E f,
    RealFloat a,
    Epsilon a
  ) =>
  -- | starting point
  f a ->
  -- | direction (no need to be normalized)
  f a ->
  -- | list of (delta time, point, [grid point]) pairs
  [(a, f a, [f Int])]
march start direction = runST do
  let fi = fromIntegral :: Int -> a
      new = newSTRef
      read = readSTRef
      write = writeSTRef
      minimum_ = foldr1 \a b ->
        if
          | isNaN a -> b
          | isNaN b -> a
          | otherwise -> min a b -- if both are NaN, then pick either
      sig = floor . signum <$> direction
      round_ = f <$> sig
        where
          f (-1) = ceiling
          f 1 = floor
          f _ = floor -- direction is zero, so it doesn't matter
  cur <- new start
  com <- new $ pure 0 -- Kahan sum compensator
  fix \this -> do
    -- mechanism:
    -- using the parametric equation of the line segment
    -- find the closest intersection with the grid -> get 'time' value
    -- then use the 'time' to get the coordinates of the intersection
    let (!) = index
        t cur' i =
          -- solve for time to next intersection in dimension i
          let s = sig ! i
              u = fi ((round_ ! i) (cur' ! i) + s) - cur' ! i
           in ( -- the time
                u / direction ! i,
                -- grid point
                \v ->
                  let roundedv = tabulate \j -> (round_ ! j) (v ! j)
                   in roundedv & el i +~ s
              )
        add c x y =
          -- Kahan's compensated sum (x += y)
          let y' = y - c
              s = x + y'
              c' = s - x - y'
           in (s, c')
    com' <- read com
    cur' <- read cur
    let times = tabulate @f $ t cur'
        nearequal = (nearZero .) . subtract
        tim = minimum_ $ fmap fst times
        sametimes = fmap snd $ filter (nearequal tim . fst) $ toList times
        vadd v w = tabulate \i ->
          -- elementwise error-compensated vector addition
          add (com' ! i) (v ! i) (w ! i)
        s = vadd cur' $ direction <&> (* tim)
        newcur = fst <$> s
        newcom = snd <$> s
    write cur newcur
    write com newcom
    ((tim, newcur, ($ newcur) <$> sametimes) :) <$> this
{-# INLINEABLE march #-}
{-# SPECIALIZE march :: V3 Double -> V3 Double -> [(Double, V3 Double, [V3 Int])] #-}
