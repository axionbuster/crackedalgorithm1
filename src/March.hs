-- | march along a line segment, finding all intersections with grid points
module March (march) where

import Control.Lens hiding (index)
import Control.Monad.Fix
import Control.Monad.ST.Lazy
import Data.Foldable
import Data.Functor
import Data.Functor.Rep
import Data.STRef.Lazy
import Linear hiding (trace)
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
      (!) = index
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
    -- 'current' location and compensator
    com' <- read com
    cur' <- read cur
    let times = tabulate @f \i ->
          -- solve for time to next intersection in dimension i
          ( -- the time
            let u = fi ((round_ ! i) (cur' ! i) + sig ! i) - cur' ! i
             in u / direction ! i,
            -- grid point (compute from later-determined cur value)
            \v ->
              let roundedv = round <$> v
               in liftA2 (-) roundedv sig & el i +~ sig ! i
          )
        tim = minimum_ $ fmap fst times
        eqtim = nearZero . subtract tim
        -- list of functions that, given a point, return the grid coordinates
        gridcoordsf = fmap snd $ filter (eqtim . fst) $ toList times
        vadd v w = tabulate \i ->
          -- elementwise error-compensated vector addition
          add (com' ! i) (v ! i) (w ! i)
          where
            add c x y =
              -- Kahan's compensated sum (x += y)
              let y' = y - c
                  u = x + y'
                  c' = u - x - y'
               in (u, c')
        -- update current position and compensator
        s = vadd cur' $ direction <&> (* tim)
        newcur = fst <$> s
        newcom = snd <$> s
        -- properly round coordinates meant to be integers
        newcur' = tabulate @f \i ->
          let n = newcur ! i
           in if eqtim $ fst $ times ! i
                then fi $ round n
                else n
    write cur newcur'
    write com newcom
    ((tim, newcur', gridcoordsf <&> ($ newcur')) :) <$> this
{-# INLINEABLE march #-}
{-# SPECIALIZE march :: V3 Double -> V3 Double -> [(Double, V3 Double, [V3 Int])] #-}
{-# SPECIALIZE march :: V3 Float -> V3 Float -> [(Float, V3 Float, [V3 Int])] #-}
