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
    let (V3 x0_ y0_ z0_, V3 x1_ y1_ z1_) = (center this, center this + moving)
        (x0, x1, y0, y1, z0, z1) =
          ( min x0_ x1_,
            max x0_ x1_,
            min y0_ y1_,
            max y0_ y1_,
            min z0_ z1_,
            max z0_ z1_
          )
        lc = locorner that
        hc = hicorner that
        d = dimensions this
        (V3 u0 v0 w0, V3 u1 v1 w1) = (lc - d ^/ 2, hc + d ^/ 2)
        (dx, dy, dz) = (x1 - x0, y1 - y0, z1 - z0)
        (V2 tx0 tx1, V2 ty0 ty1, V2 tz0 tz1) =
          ( (V2 u0 u1 - pure x0) ^/ dx,
            (V2 v0 v1 - pure y0) ^/ dy,
            (V2 w0 w1 - pure z0) ^/ dz
          )
        -- if any NaNs exist, the box is not intersecting
        -- on some axis
        nonans = all (not . isNaN)
        -- entering time, each axis, and t = 0
        times0 = V4 tx0 ty0 tz0 0
        -- exiting time, each axis, and t = 1
        times1 = V4 tx1 ty1 tz1 1
     in -- it is absurd to have a maximum time less than the minimum time
        -- on any axis, and in all other cases, the box is intersecting
        nonans times0 && nonans times1 && maximum times0 < minimum times1
  intersecting this that =
    let lotest = and $ mzipWith (<) (locorner this) (hicorner that)
        hitest = and $ mzipWith (>) (hicorner this) (locorner that)
     in lotest && hitest
  relative box = V2 (locorner box) (hicorner box)
  translate displacement box = box {center = displacement + center box}

boxzero :: (Num a) => Box a
boxzero = Box zero zero

locorner :: (Fractional a) => Box a -> V3 a
locorner (Box d c) = c - d ^/ 2

hicorner :: (Fractional a) => Box a -> V3 a
hicorner (Box d c) = c + d ^/ 2
