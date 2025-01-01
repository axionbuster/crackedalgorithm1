{-# LANGUAGE MonoLocalBinds #-}

-- | pure collision detection and resolution
module Collision
  ( Shape (..),
    SomeShape1 (..),
    Hit (..),
    Box (..),
    ManyBoxes (..),
    _dimensions,
    _center,
    _lcorner,
    _hcorner,
    boxfromcorners,
    castshape1,
    boxzero,
    hicorner,
    locorner,
    shicorner,
    slocorner,
  )
where

import Control.Lens hiding (index)
import Control.Monad.Zip
import Data.Data
import Data.Foldable
import Data.Functor.Rep
import Data.Hashable
import Data.Maybe
import GHC.Generics (Generic)
import Linear

-- | a collision resolution data type
data Hit a = Hit
  { -- | proportion of move completed in [0, 1]
    hittime :: !a,
    -- | the point of collision
    --
    -- if you're using 'Box', this is the center of the box
    hitwhere :: !(V3 a),
    -- | normal vector of the surface hit
    --
    -- a signum vector, so each component is either -1, 0, or 1
    hitnorm :: !(V3 a)
  }
  deriving (Show, Eq, Generic, Typeable, Hashable, Functor, Data)

-- | existential 'Shape' type but where numeric type is erased
--
-- see also: 'castshape1'
data SomeShape1 a
  = forall s.
    ( Typeable (s a),
      Show (s a),
      Shape s
    ) =>
    SomeShape1 (s a)
  deriving (Typeable)

instance Show (SomeShape1 a) where
  show (SomeShape1 s) = show s

instance Shape SomeShape1 where
  intersecting (SomeShape1 s1) (SomeShape1 s2)
    | Just s3 <- cast s2 = intersecting s1 s3
    | otherwise = intersecting (tomanyboxes s1) (tomanyboxes s2)
  hitting v (SomeShape1 s1) (SomeShape1 s2)
    | Just s3 <- cast s2 = hitting v s1 s3
    | otherwise = hitting v (tomanyboxes s1) (tomanyboxes s2)
  translate v (SomeShape1 s) = SomeShape1 (translate v s)
  corners (SomeShape1 s) = corners s
  tomanyboxes (SomeShape1 s) = tomanyboxes s

-- | cast a 'SomeShape1' to a specific type
castshape1 :: (Typeable b) => SomeShape1 a -> Maybe b
castshape1 (SomeShape1 s) = cast s

-- | an AABB type class used for collision detection and resolution
class Shape s where
  -- | check if two shapes intersect
  intersecting :: (Fractional a, Ord a) => s a -> s a -> Bool

  -- | check if the first shape will collide into the second shape
  -- if it moves with the given displacement
  hitting :: (RealFloat a) => V3 a -> s a -> s a -> Maybe (Hit a)

  -- | translate the shape by the given displacement
  translate :: (Num a) => V3 a -> s a -> s a

  -- | the locations of the lower and higher corners of the shape
  -- respectively
  corners :: (Fractional a, Ord a) => s a -> V2 (V3 a)

  -- | convert a 'Shape' to a 'ManyBoxes' of 'Box'es with a list container,
  -- which is a canonical form for 'ManyBoxes'
  tomanyboxes :: s a -> ManyBoxes [] a

  -- | the center of the shape
  scenter :: (Fractional a, Ord a) => s a -> V3 a
  scenter s = (sum . corners $ s) <&> (/ 2) -- not robust to large numbers
  {-# INLINE scenter #-}

  -- | the dimensions of the shape
  sdimensions :: (Fractional a, Ord a) => s a -> V3 a
  sdimensions s = let V2 l h = corners s in h - l
  {-# INLINE sdimensions #-}

v2fst :: V2 a -> a
v2fst (V2 a _) = a

v2snd :: V2 a -> a
v2snd (V2 _ b) = b

-- | the upper corner of a shape
shicorner :: (Shape s, Fractional a, Ord a) => s a -> V3 a
shicorner = v2snd . corners
{-# INLINE shicorner #-}

-- | the lower corner of a shape
slocorner :: (Shape s, Fractional a, Ord a) => s a -> V3 a
slocorner = v2fst . corners
{-# INLINE slocorner #-}

-- | a box in 3D space, located either relatively or absolutely
data Box a = Box
  { -- | the dimensions of the box
    dimensions :: !(V3 a),
    -- | the center of the box
    center :: !(V3 a)
  }
  deriving stock (Show, Eq, Generic, Typeable, Functor, Data)
  deriving anyclass (Hashable)

-- | a box from the low and high corners
boxfromcorners ::
  (Fractional a) =>
  -- | low corner
  V3 a ->
  -- | high corner
  V3 a ->
  -- | the box
  Box a
boxfromcorners l h = Box (h - l) ((h + l) ^/ 2)

-- | a newtype over a 'Foldable' 'Functor' container of 'Box'es
--
-- the low and high corners are those of the smallest bounding box
newtype ManyBoxes f a = ManyBoxes (f (Box a))
  deriving stock (Generic, Typeable)

deriving stock instance
  (Typeable f, Typeable a, Data (f (Box a))) =>
  Data (ManyBoxes f a)

instance (Eq (f (Box a))) => Eq (ManyBoxes f a) where
  ManyBoxes a == ManyBoxes b = a == b
  {-# INLINE (==) #-}

instance (Ord (f (Box a))) => Ord (ManyBoxes f a) where
  compare (ManyBoxes a) (ManyBoxes b) = compare a b
  {-# INLINE compare #-}

instance (Functor f) => Functor (ManyBoxes f) where
  fmap f (ManyBoxes boxes) = ManyBoxes $ fmap g boxes
    where
      g (Box d c) = Box (f <$> d) (f <$> c)
  {-# INLINE fmap #-}

instance (Hashable (f (Box a))) => Hashable (ManyBoxes f a) where
  hashWithSalt s (ManyBoxes boxes) = hashWithSalt s boxes
  {-# INLINE hashWithSalt #-}

-- | Lens for the dimensions of the box
_dimensions :: Lens' (Box a) (V3 a)
_dimensions = lens dimensions \b d -> b {dimensions = d}
{-# INLINE _dimensions #-}

-- | Lens for the center of the box
_center :: Lens' (Box a) (V3 a)
_center = lens center \b c -> b {center = c}
{-# INLINE _center #-}

-- | Lens for the lower corner of the box
_lcorner :: (Fractional a) => Lens' (Box a) (V3 a)
_lcorner = lens locorner \b l -> b {center = l + dimensions b ^/ 2}
{-# INLINE _lcorner #-}

-- | Lens for the higher corner of the box
_hcorner :: (Fractional a) => Lens' (Box a) (V3 a)
_hcorner = lens hicorner \b h -> b {center = h - dimensions b ^/ 2}
{-# INLINE _hcorner #-}

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
        nonans = not . any isNaN
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
                { hittime = t,
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
  translate displacement box = box {center = displacement + center box}
  corners box = V2 (locorner box) (hicorner box)
  tomanyboxes = ManyBoxes . pure
  scenter = center
  sdimensions = dimensions

-- | a box with zero dimensions and center
boxzero :: (Num a) => Box a
boxzero = Box zero zero

-- | the location of the lower corner of the box
locorner :: (Fractional a) => Box a -> V3 a
locorner (Box d c) = c - d ^/ 2

-- | the location of the higher corner of the box
hicorner :: (Fractional a) => Box a -> V3 a
hicorner (Box d c) = c + d ^/ 2

instance (Show (f (Box a))) => Show (ManyBoxes f a) where
  show (ManyBoxes boxes) = show boxes

instance (Functor f, Foldable f) => Shape (ManyBoxes f) where
  -- find the first hitting collision
  hitting moving (ManyBoxes these) (ManyBoxes those) =
    let minBy f x y = if f x < f y then x else y
        minimum' =
          Nothing & foldl' \case
            Nothing -> Just
            Just x -> Just . minBy hittime x
        nearest f = minimum' . mapMaybe f . toList
        firsthit boxes box = nearest (hitting moving box) boxes
     in nearest (firsthit those) these

  -- check if any of the boxes intersect
  intersecting (ManyBoxes these) (ManyBoxes those) =
    foldr
      ( \this r ->
          foldr
            do \that s -> intersecting this that || s
            do False
            do those
            || r
      )
      do False
      do these

  -- translate all the boxes
  translate displacement (ManyBoxes boxes) =
    ManyBoxes $
      translate displacement <$> boxes

  -- find the corners of the smallest bounding box
  corners (ManyBoxes boxes) =
    let low = foldl' (flip $ liftA2 min . locorner) (pure (1 / 0)) boxes
        high = foldl' (flip $ liftA2 max . hicorner) (pure - (1 / 0)) boxes
     in V2 low high

  tomanyboxes (ManyBoxes boxes) = ManyBoxes $ toList boxes
