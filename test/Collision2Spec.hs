module Collision2Spec (spec) where

import Test.Hspec
import Linear
import Collision
import Data.Maybe

spec :: Spec
spec = do
  describe "Collision2" do
    describe "resolve" do
      it "passes case 1" do
        let stone = error "stone block at 0, 43, 1"
            zombie = ManyBoxes [Box (V3 0.5 0 0) (V3 1 1 1)]
         in error "not implemented" `shouldBe` "implemented"
