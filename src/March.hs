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
      round_ (-1) = ceiling
      round_ 1 = floor
      round_ _ = floor -- direction is zero, so it doesn't matter
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
            let r = round_ $ sig ! i
                u = fi (r (cur' ! i) + sig ! i) - cur' ! i
             in u / direction ! i,
            -- grid point (compute from later-determined cur value)
            -- this was the hardest part to figure out
            \v ->
              -- 1
              -- on (round_ (-(sig ! i))) ... you need a greatest integer
              -- less than (or least integer greater than) the current
              -- coordinate, which is either ((subtract 1) . ceiling)
              -- or ((+ 1) . floor), depending on the the OPPOSITE side of the
              -- signum of the direction. the subtract 1 vs. + 1 will be done
              -- when we subtract the sig component from the grid point
              -- later on
              --
              -- 2
              -- on (max 0 <$> sig) ... if you flip the coordinate system
              -- by some axis, the grid points are still numbered by the
              -- bottom left (& etc) corner, so you need to subtract 1
              -- from the grid point if the direction is negative. this
              -- means to add 1 to the sig component -> hence max 0
              let roundedv = tabulate \j -> (round_ (-(sig ! j))) (v ! j)
               in liftA2 (-) roundedv (max 0 <$> sig) & el i +~ sig ! i
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
