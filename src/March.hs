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

-- | convert negative zero to positive zero
nonegzero :: (RealFloat a) => a -> a
nonegzero x | isNegativeZero x = 0 -- positive zero
nonegzero x = x

isfinite :: (RealFloat a) => a -> Bool
isfinite x = not (isNaN x || isInfinite x)

-- | apply Kahan's compensated sum to two numbers
add ::
  (Num a) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | compensator
  a ->
  -- | (x + y, compensator)
  (a, a)
add x y c =
  let y' = y - c
      u = x + y'
      c' = u - x - y'
   in (u, c')

-- | march along a line segment, finding all intersections
-- with grid squares or cubes (depending on the dimensionality)
-- as well as the time it takes to reach each intersection
-- and the cubes that are intersected
--
-- the cubes are represented by their bottom-left corner
--
-- in 2D, when a point is intersected, it will return the two squares
-- that are intersected by the line segment. so note that it doesn't
-- include the diagonal square
--
-- in 3D, it will return three cubes in the same way. no diagonal cube
--
-- a compensated sum is used to reduce floating point error
--
-- the returned list being infinite, it is recommended to
-- use 'take' to limit the number of points to be computed
--
-- if the direction is (near) zero, or if any component of the
-- direction is not finite, then the function will return an empty list
march ::
  forall f a.
  ( Foldable f,
    Representable f,
    Rep f ~ E f,
    RealFloat a,
    Epsilon a
  ) =>
  -- | starting point
  f a ->
  -- | direction (no need to be normalized)
  f a ->
  -- | list of (total time, point, [grid point]) pairs
  [(a, f a, [f Int])]
march _ direction | any (not . isfinite) direction = []
march _ direction | all nearZero direction = []
march start (fmap nonegzero -> direction) = runST do
  let fi = fromIntegral :: Int -> a
      (!) = index
      new = newSTRef
      read = readSTRef
      write = writeSTRef
      modify = modifySTRef
      lift2 f x y = tabulate @f \i -> f (x ! i) (y ! i)
      minimum_ = foldr1 \a b ->
        if
          | isNaN a -> b
          | isNaN b -> a
          | otherwise -> min a b -- if both are NaN, then pick either
      computesig d = f . floor . signum <$> d
        where
          -- for difficult-to-explain reasons, you need to give a stationary (0)
          -- displacement component a fake signum of +1 in order to avoid
          -- getting -Infinity when it shouldn't
          -- ^ FIXME: this explanation could be outdated
          f 0 = 1
          f x = x
      sig = computesig direction
      -- round toward opposite direction of a signum component
      round_ (-1) = ceiling
      round_ 1 = floor
      round_ _ = error "signum neither -1 nor 1"
      -- vector of functions that each generate a grid point
      -- (specialized for each dimension). confused yet? yeah, it's
      -- really hard to explain
      gengridpoints = tabulate \i sig' v ->
        -- grid point (compute from later-determined cur value)
        -- this was the hardest part to figure out
        --
        -- 1
        -- on (round_ (-(sig' ! i))) ... you need a greatest integer
        -- less than (or least integer greater than) the current
        -- coordinate, which is either ((subtract 1) . ceiling)
        -- or ((+ 1) . floor), depending on the the OPPOSITE side of the
        -- signum of the direction. the subtract 1 vs. + 1 will be done
        -- when we subtract the sig' component from the grid point
        -- later on
        --
        -- 2
        -- on (max 0 <$> sig') ... if you flip the coordinate system
        -- by some axis, the grid points are still numbered by the
        -- bottom left (& etc) corner, so you need to subtract 1
        -- from the grid point if the direction is negative. this
        -- means to add 1 to the sig' component -> hence max 0
        let roundedv = tabulate \j -> round_ (-(sig' ! j)) (v ! j)
         in lift2 (-) roundedv (max 0 <$> sig') & el i +~ sig' ! i
  ftm <- new True -- first time marker
  cur <- new start
  com <- new $ tabulate $ const 0 -- Kahan sum compensator for cur
  tot <- new (0, 0) -- (total time, compensator)
  fix \this -> do
    -- mechanism:
    -- using the parametric equation of the line segment
    -- find the closest intersection with the grid -> get 'time' value
    -- then use the 'time' to get the coordinates of the intersection
    -- 'current' location and compensator
    com' <- read com
    cur' <- read cur
    (sig', dir) <-
      -- IMPORTANT NOTE:
      -- at start, we go BACKWARD and find the first intersection
      -- and then go forward from there to resume the normal process
      -- this extremely hacky way of doing things is necessary
      -- to generate the grid coordinates of the starting point in a
      -- way that is consistent with the rest of the function
      read ftm <&> \ftm' ->
        if ftm'
          then
            -- we can't just reverse the "signum" vector because it's
            -- actually not a signum vector; we start from the signum
            -- vector, but we coerce 0 to 1. so the opposite of
            -- this vector has to be computed manually from reverse
            let d = (* (-1)) <$> direction
             in (computesig d, d)
          else (sig, direction)
    let times = tabulate @f \i ->
          -- solve for time to next intersection in dimension i
          ( let r = round_ $ sig' ! i
                u = fi (r (cur' ! i) + sig' ! i) - cur' ! i
             in u / dir ! i, -- the time
            (gengridpoints ! i) sig' -- function to get grid point
          )
        tim = minimum_ $ filter (> 0) $ toList $ fmap fst times
        eqtim = nearZero . subtract tim
        -- list of functions that, given a point, return the grid coordinates
        gridcoordsf = fmap snd $ filter (eqtim . fst) $ toList times
        vadd v w = tabulate \i ->
          -- elementwise error-compensated vector addition
          add (v ! i) (w ! i) (com' ! i)
        -- update current position and compensator
        s = vadd cur' $ dir <&> (* tim)
        newcur = fst <$> s
        newcom = snd <$> s
        -- properly round coordinates meant to be integers
        newcur' = tabulate @f \i ->
          let n = newcur ! i
           in if eqtim $ fst $ times ! i
                then fi $ round n
                else n
    -- update total time with compensated sum
    tot' <- modify tot (uncurry (add tim)) *> fmap fst (read tot)
    write cur newcur'
    write com newcom
    ftm' <- read ftm
    if ftm'
      then do
        -- the direction will be reset as we go forward
        write ftm False
        -- negate the time so that next time we add it back it becomes 0
        modify tot (\(x, y) -> (x * (-1), y * (-1)))
        -- we ignore this backward step
        this
      else ((tot', newcur', gridcoordsf <&> ($ newcur')) :) <$> this
{-# INLINEABLE march #-}
{-# SPECIALIZE march :: V3 Double -> V3 Double -> [(Double, V3 Double, [V3 Int])] #-}
{-# SPECIALIZE march :: V3 Float -> V3 Float -> [(Float, V3 Float, [V3 Int])] #-}
