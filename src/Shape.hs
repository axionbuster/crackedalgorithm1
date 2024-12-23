{-# LANGUAGE MonoLocalBinds #-}

-- | a module for defining the @Shape@ typeclass
module Shape (Shape (..), SomeShape1 (..), castshape1, Hit (..)) where

import Data.Data
import Linear (V2, V3)

-- | a collision resolution data type
data Hit a = Hit
  { -- | proportion of move completed in [0, 1]
    hitprop :: a,
    -- | normal vector of the surface hit
    --
    -- a signum vector, so each component is either -1, 0, or 1
    hitnorm :: V3 a,
    -- | exact meaning and purpose unknown
    hitcollidedshapexyz :: V3 a,
    -- | what shape was hit (?)
    hitcollidedshape :: SomeShape1 a
  }
  deriving (Show)

-- | existential 'Shape' type but numeric type is erased
data SomeShape1 a
  = forall s.
    ( Typeable (s a),
      Show (s a),
      Shape s
    ) =>
    SomeShape1 (s a)

instance Show (SomeShape1 a) where
  show (SomeShape1 s) = show s

-- | cast a 'SomeShape1' to a specific type
castshape1 :: (Typeable b) => SomeShape1 a -> Maybe b
castshape1 (SomeShape1 s) = cast s

-- | a translation of the @Shape@ interface from the original code
class Shape s where
  -- | check if two shapes intersect
  intersecting :: (Fractional a, Ord a) => s a -> s a -> Bool

  -- | check if the first shape will collide into the second shape
  -- if it moves with the given displacement
  --
  -- TODO: this should be a method of the 'Maybe Hit' type
  hitting :: (Show a, RealFloat a) => V3 a -> s a -> s a -> Bool

  -- | return the relative starting and ending positions of the shape
  -- respectively
  relative :: (Fractional a) => s a -> V2 (V3 a)

  -- | translate the shape by the given displacement
  translate :: (Num a) => V3 a -> s a -> s a
