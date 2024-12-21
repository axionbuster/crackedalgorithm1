module Lib (march) where

import Control.Monad.Fix
import Control.Monad.ST.Lazy
import Control.Monad.Zip
import Data.Functor.Rep
import Data.STRef.Lazy
import Prelude hiding (read)

-- | March along a line segment, finding all intersections
-- with grid points
march ::
  forall f a.
  ( RealFrac a,
    Foldable f,
    Representable f,
    MonadZip f
  ) =>
  f a -> f a -> [f a]
march start direction = runST do
  let fi = fromIntegral :: Int -> a
      new = newSTRef
      read = readSTRef
      modify = modifySTRef
  cur <- new start
  sig <- new $ signum <$> direction
  fix \this -> do
    let t cur' sig' i =
          let nex' = mzipWith (\c s -> (fi . floor) c + s) cur' sig'
           in (index nex' i - index cur' i) / index direction i
    tim <- do
      cur' <- read cur
      sig' <- read sig
      pure $ minimum $ tabulate @f (t cur' sig')
    modify cur (liftA2 (+) $ (tim *) `fmap` direction)
    cur' <- read cur
    (cur' :) <$> this
