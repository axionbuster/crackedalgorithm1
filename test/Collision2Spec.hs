module Collision2Spec (spec) where

import Test.Hspec
import Linear
import Shape
import Box
import Data.Maybe
import Collision
import Collision2

spec :: Spec
spec = do
  describe "Collision2" do
    describe "resolve" do
      it "passes case 1" do
        let stone = error "stone block at 0, 43, 1"
            zombie = ManyBoxes [] (Box (V3 0.5 ))
