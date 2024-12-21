-- | march along a line segment, finding all intersections with grid points
module Lib (march) where

import Control.Monad.Fix
import Control.Monad.ST.Lazy
import Data.Functor
import Data.Functor.Rep
import Data.STRef.Lazy
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
    RealFloat a
  ) =>
  -- | starting point
  f a ->
  -- | direction (no need to be normalized)
  f a ->
  -- | list of (delta time, point) pairs
  [(a, f a)]
march start direction = runST do
  let fi = fromIntegral :: Int -> a
      new = newSTRef
      read = readSTRef
      write = writeSTRef
      minnonan a b
        | isNaN a = b
        | isNaN b = a
        | otherwise = min a b -- if both are NaN, then pick either
      minimum_ = foldr1 minnonan
  cur <- new start
  com <- new $ pure 0 -- Kahan sum compensator
  let sig = signum <$> direction
  fix \this -> do
    -- mechanism:
    -- using the parametric equation of the line segment
    -- find the closest intersection with the grid -> get 'time' value
    -- then use the 'time' to get the coordinates of the intersection
    let (!) = index
        t cur' i =
          -- solve for time to next intersection
          let s = fi (floor $ cur' ! i) + sig ! i
           in (s - cur' ! i) / direction ! i
        add c x y =
          -- Kahan's compensated sum (x += y)
          let y' = y - c
              s = x + y'
              c' = (s - x) - y'
           in (s, c')
    com' <- read com
    cur' <- read cur
    let tim = minimum_ $ tabulate @f $ t cur'
        vadd v w = tabulate @f \i ->
          -- elementwise error-compensated vector addition
          add (com' ! i) (v ! i) (w ! i)
        s = vadd cur' $ direction <&> (* tim)
        newcur = fst <$> s
        newcom = snd <$> s
    write cur newcur
    write com newcom
    ((tim, newcur) :) <$> this
