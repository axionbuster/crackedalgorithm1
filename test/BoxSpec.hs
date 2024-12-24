module BoxSpec (spec) where

import Test.Hspec
import Linear
import Shape
import Box

spec :: Spec
spec = do
  describe "Box" $ do
    describe "boxzero" $ do
      it "creates a box with zero dimensions and center" $ do
        let box = boxzero
        center box `shouldBe` zero
        dimensions box `shouldBe` zero

    describe "intersecting" $ do
      it "detects when boxes intersect" $ do
        let box1 = Box (V3 0 0 0) (V3 1 1 1)
            box2 = Box (V3 0.5 0.5 0.5) (V3 1 1 1)
        intersecting box1 box2 `shouldBe` True