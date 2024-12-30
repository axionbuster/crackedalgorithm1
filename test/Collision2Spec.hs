module Collision2Spec (spec) where

import BlockModel1
import Collision
import Collision2
import Control.Category ((>>>))
import Data.Map.Strict as M
import Effectful
import Linear
import Test.Hspec

-- | given low coordinates return unit cube
genericcube :: V3 Double -> Box Double
genericcube l = Box (pure 1) (l + pure 0.5) -- dim, cent

-- | given low coordinates return zombie
genericzombie :: V3 Double -> Box Double
genericzombie =
  (+ V3 0.3 0 0.3)
    >>> (`translate` boxfromcorners (V3 (-0.3) 0 (-0.3)) (V3 0.3 1.95 0.3))

-- | given a list of coordinates return a model of stones
stones :: [V3 Int] -> Model Box Double
stones = Model . M.fromList . fmap \v -> (v, genericcube (fromIntegral <$> v))

spec :: Spec
spec = do
  describe "internal" do
    describe "genericzombie" do
      it "works" do
        genericzombie zero
          `shouldBe` Box
            { dimensions = V3 0.6 1.95 0.6,
              center = V3 0.3 0.975 0.3
            }
  describe "Collision2" do
    describe "resolve" do
      it "passes case 1" do
        let model = stones [V3 0 43 1]
            zombie = genericzombie (V3 0 42 0)
            disp = V3 0 0 10
            e = runPureEff . runBlockModel model $ resolve zombie disp
         in e
              `shouldBe`
              -- dummy resolve
              Resolve
                { -- don't ask me why the position is 0.7
                  -- i just got it from the original code
                  respos = V3 0 42 0.7,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = EQ}
                }
