module Lib (march) where

import Control.Monad.Fix
import Control.Monad.ST.Lazy
import Data.Functor.Rep
import Data.STRef.Lazy
import Prelude hiding (read)

-- | march along a line segment, finding all intersections
-- with grid points
--
-- the first point is always the starting point
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
  -- | list of points
  [f a]
march start direction = runST do
  let fi = fromIntegral :: Int -> a
      new = newSTRef
      read = readSTRef
      modify = modifySTRef
      minnonan a b
        | isNaN a = b
        | isNaN b = a
        | otherwise = min a b -- if both are NaN, then pick any
  cur <- new start
  sig <- new $ signum <$> direction
  fix \this -> do
    let t cur' sig' i =
          let nex' = tabulate @f \j ->
                fi (floor $ cur' `index` j) + sig' `index` j
           in (index nex' i - index cur' i) / index direction i
    tim <- do
      cur' <- read cur
      sig' <- read sig
      pure $ foldr1 minnonan $ tabulate @f (t cur' sig')
    cur' <- read cur
    modify cur (liftA2 (+) $ (tim *) `fmap` direction)
    (cur' :) <$> this
