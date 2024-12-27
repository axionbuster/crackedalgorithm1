{-# LANGUAGE MonoLocalBinds #-}

-- | a module for defining the @Shape@ typeclass
module Shape (Shape (..), SomeShape1 (..), castshape1, Hit (..)) where

import Data.Data
import Linear (V2, V3)

-- | a collision resolution data type
data Hit a = Hit
  { -- | proportion of move completed in [0, 1]
    hitprop :: a,
    -- | the point of collision
    hitwhere :: V3 a,
    -- | normal vector of the surface hit
    --
    -- a signum vector, so each component is either -1, 0, or 1
    hitnorm :: V3 a
  }
  deriving (Show)

-- | existential 'Shape' type but numeric type is erased
--
-- The 'Shape' instance for 'SomeShape1' requires for 'intersecting'
-- and 'hitting' the two underlying shapes to have the same type
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
    | otherwise = error "intersecting: type mismatch"
  hitting v (SomeShape1 s1) (SomeShape1 s2)
    | Just s3 <- cast s2 = hitting v s1 s3
    | otherwise = error "hitting: type mismatch"
  translate v (SomeShape1 s) = SomeShape1 (translate v s)
  corners (SomeShape1 s) = corners s

-- | cast a 'SomeShape1' to a specific type
castshape1 :: (Typeable b) => SomeShape1 a -> Maybe b
castshape1 (SomeShape1 s) = cast s

-- | a translation of the @Shape@ interface from the original code
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
