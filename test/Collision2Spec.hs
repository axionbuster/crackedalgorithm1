module Collision2Spec (spec) where

import BlockModel1
import Collision
import Collision2
import Data.Map.Strict as M
import Effectful
import Linear
import Test.Hspec

-- | given low coordinates return unit cube
genericcube :: V3 Double -> Box Double
genericcube l = Box (pure 1) (l + pure 0.5) -- dim, cent

-- | given low coordinates return zombie
genericzombie :: V3 Double -> Box Double
genericzombie = (`translate` Box (V3 0.6 1.95 0.6) (V3 0.3 0.975 0.3))

-- | get the center of a zombie with the given low coordinates
zomtr :: V3 Double -> V3 Double
zomtr = (+ V3 0.3 0.975 0.3)

-- | given a list of coordinates return a model of stones
stones :: [V3 Int] -> Model Box Double
stones = Model . M.fromList . fmap \v -> (v, genericcube (fromIntegral <$> v))

-- | run an effect with a model
run :: Model f n -> Eff (GetBlock f n : '[]) a -> a
run = (runPureEff .) . runBlockModel

spec :: Spec
spec = do
  describe "Collision2" do
    describe "resolve" do
      it "blocks zombie from sliding right" do
        let model = stones [V3 0 43 1]
            zombie = genericzombie (V3 0 42 0)
            disp = V3 0 0 10
         in run model (resolve zombie disp)
              `shouldBe` Resolve
                -- the zombie tries to slide to the right but gets
                -- blocked by the stone
                { respos = zomtr $ V3 0 42 0.4,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = EQ}
                }
      it "does not block zombie from sliding right when not blocking" do
        let model = stones [V3 0 42 1]
            zombie = genericzombie (V3 0 42 0.9)
            disp = V3 0 0 1.3
         in run model (resolve zombie disp)
              `shouldBe` Resolve
                -- because the stone is on the same level as the zombie
                -- it does not block it; the zombie slides through
                { respos = zomtr $ V3 0 42 1,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = EQ}
                }
