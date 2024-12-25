-- | a simple 3D box
module Box
  ( Box (..),
    _dimensions,
    _center,
    boxzero,
    locorner,
    hicorner,
  )
where

import Control.Lens hiding (index)
import Control.Monad.Zip
import Data.Functor.Rep
import Linear hiding (trace)
import Shape

-- | a box in 3D space, located either relatively or absolutely
data Box a = Box
  { -- | the dimensions of the box
    dimensions :: !(V3 a),
    -- | the center of the box
    center :: !(V3 a)
  }
  deriving stock (Show)

-- | Lens for the dimensions of the box
_dimensions :: Lens' (Box a) (V3 a)
_dimensions = lens dimensions \b d -> b {dimensions = d}

-- | Lens for the center of the box
_center :: Lens' (Box a) (V3 a)
_center = lens center \b c -> b {center = c}

instance Shape Box where
  -- moving = displacement from t = 0 to t = 1
  -- 'this' is the box that is moving
  -- into 'that' box
  hitting moving this that =
    let (V3 x0 y0 z0) = center this
        l = locorner that
        h = hicorner that
        d = dimensions this
        (V3 u0 v0 w0, V3 u1 v1 w1) = (l - d ^/ 2, h + d ^/ 2)
        (V3 dx dy dz) = moving
        -- entering and exiting times for each axis
        (V2 tx0 tx1, V2 ty0 ty1, V2 tz0 tz1) =
          ( (V2 u0 u1 - pure x0) ^/ dx,
            (V2 v0 v1 - pure y0) ^/ dy,
            (V2 w0 w1 - pure z0) ^/ dz
          )
        -- sort the times so that the entering time is first
        -- in each axis
        v2sort (V2 a b) = if a < b then V2 a b else V2 b a
        (V2 sx0 sx1, V2 sy0 sy1, V2 sz0 sz1) =
          ( v2sort (V2 tx0 tx1),
            v2sort (V2 ty0 ty1),
            v2sort (V2 tz0 tz1)
          )
        -- if any NaNs exist, the box is not intersecting
        -- on some axis, thus the box is not intersecting
        nonans = all (not . isNaN)
        -- entering time, each axis, and t = 0
        times0 = V4 sx0 sy0 sz0 0
        -- exiting time, each axis, and t = 1
        times1 = V4 sx1 sy1 sz1 1
        -- time of hit
        t = maximum times0
     in -- it is absurd to be exiting earlier than entering
        -- but in all other cases, the box is intersecting
        if nonans times0 && nonans times1 && t < minimum times1
          then
            -- we have a hit!
            Just
              Hit
                { hitprop = t,
                  hitwhere = center this + t *^ moving,
                  hitnorm =
                    let p1m1 x = if x then 1 else (-1)
                        times0_ = times0 ^. _xyz
                     in tabulate \i ->
                          -- if collision in this axis
                          -- then opposite of the direction of movement
                          -- that's the normal!
                          -- or else nothing so 0
                          if t == times0_ `index` i
                            then p1m1 $ moving `index` i < 0
                            else 0
                }
          else Nothing
  intersecting this that =
    let lotest = and $ mzipWith (<) (locorner this) (hicorner that)
        hitest = and $ mzipWith (>) (hicorner this) (locorner that)
     in lotest && hitest
  relative box = V2 (locorner box) (hicorner box)
  translate displacement box = box {center = displacement + center box}

-- | a box with zero dimensions and center
boxzero :: (Num a) => Box a
boxzero = Box zero zero

-- | the location of the lower corner of the box
locorner :: (Fractional a) => Box a -> V3 a
locorner (Box d c) = c - d ^/ 2

-- | the location of the higher corner of the box
hicorner :: (Fractional a) => Box a -> V3 a
hicorner (Box d c) = c + d ^/ 2
