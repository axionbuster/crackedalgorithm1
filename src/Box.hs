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

import Control.Lens
import Control.Monad.Zip
import Debug.Trace
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
    let (V3 x0 y0 z0, V3 x1 y1 z1) = (center this, moving + center this)
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
     in -- it is absurd to be exiting earlier than entering
        -- but in all other cases, the box is intersecting
        nonans times0 && nonans times1 && do
          seq
            do show (times0, times1)
            do maximum times0 < minimum times1
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

dbgbox0 :: Box Double
dbgbox0 = Box (V3 1 1 1) (V3 0.5 0.5 0.5)

dbgbox1 :: Box Double
dbgbox1 = Box (V3 1 1 1) (V3 2.5 1.5 0.5)

dbgbox2 :: Box Double
dbgbox2 = Box (pure 1) (V3 (-2.5) 1.5 0.5)

dbgbox3 :: Box Double
dbgbox3 = Box (pure 1) (V3 0.5 0.5 (-1.5))

dbgpair01 :: V3 Double -> Bool
dbgpair01 x = hitting x dbgbox0 dbgbox1

dbgpair02 :: V3 Double -> Bool
dbgpair02 x = hitting x dbgbox0 dbgbox2

dbgpair32 :: V3 Double -> Bool
dbgpair32 x = hitting x dbgbox3 dbgbox2
