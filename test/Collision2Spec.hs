module Collision2Spec (spec) where

import BlockModel1
import Collision
import Collision2
import Data.Map.Strict as M
import Data.Maybe
import Effectful
import Linear
import Test.Hspec

-- | given low coordinates return unit cube
genericcube :: V3 Double -> Box Double
genericcube l = Box (pure 1) (l + pure 0.5) -- dim, cent

-- | given low coordinates return zombie
genericzombie :: V3 Double -> Box Double
genericzombie = (`translate` Box (V3 0.6 1.95 0.6) (V3 0.3 0.975 0.3))

zombiebycenter :: V3 Double -> Box Double
zombiebycenter = Box (V3 0.6 1.95 0.6)

-- | get the center of a zombie with the given low coordinates
zomtr :: V3 Double -> V3 Double
zomtr = (+ V3 0.3 0.975 0.3)

-- | given a list of coordinates return a model of stones
stones :: [V3 Int] -> Model Box Double
stones = Model . M.fromList . fmap \v -> (v, genericcube (fromIntegral <$> v))

-- | given a list of coordinates & boxes return a model
--
-- no validation is done
boxes :: [(V3 Int, Box Double)] -> Model Box Double
boxes = Model . M.fromList

-- | run an effect with a model
run :: Model f n -> Eff (GetBlock f n : '[]) a -> a
run = (runPureEff .) . runBlockModel

-- | check if two resolutions are near equal
resolveneareq :: Resolve Double -> Resolve Double -> Expectation
resolveneareq a_ b = shouldSatisfy a_ \a ->
  nearZero (respos a - respos b)
    && nearZero (resdis a - resdis b)
    && restou a == restou b

spec :: Spec
spec = do
  describe "Collision" do
    describe "hitting" do
      it "lets zombie slide on top of block" do
        let zombie = genericzombie (V3 0 0.5 0)
            block = genericcube (V3 0 (-1) 1)
            disp = V3 0 0 1
         in hitting disp zombie block `shouldSatisfy` isNothing
  describe "Collision2" do
    describe "resolve" do
      it "blocks zombie from sliding right" do
        let model = stones [V3 0 43 1]
            zombie = genericzombie (V3 0 42 0)
            disp = V3 0 0 10
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { -- the zombie tries to slide to the right but gets
                  -- blocked by the stone
                  respos = zomtr $ V3 0 42 0.4,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = EQ}
                }
      it "detects when stuck and overlapping" do
        let model = stones [V3 0 42 1]
            zombie = genericzombie (V3 0 42 0.9)
            disp = V3 0 0 1.3
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { -- zombie is stuck because it is overlapping with the stone
                  respos = zomtr $ V3 0 42 0.9,
                  resdis = disp, -- so it can unstuck itself
                  restou = NewlyTouchingGround {newonground = EQ}
                }
      it "does well with slabs" do
        let model = boxes [(V3 0 (-1) 0, Box (V3 1 0.5 1) (V3 0.5 0.25 0.5))]
            zombie = genericzombie (V3 0 1.5 0)
            disp = V3 0 (-2) 0
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { respos = zomtr $ V3 0 0.5 0,
                  resdis = zero,
                  -- it touches the ground so it should say GT
                  restou = NewlyTouchingGround {newonground = GT}
                }
      it "slams and slides" do
        let model = stones [V3 0 0 0]
            zombie = zombiebycenter (V3 1.112 1.998 0)
            disp = V3 0.273 (-0.0784) 0
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { respos = V3 1.385 1.975 0,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = GT}
                }
      it "detects a fence below" do
        let model = boxes [(V3 0 0 0, Box (V3 0.25 1.5 0.25) (V3 0.5 0.75 0.5))]
            zombie = zombiebycenter (V3 0.8 2.475 0.8)
            disp = V3 0 (-0.25) 0
            resu = run model (resolve zombie disp)
         in resu `shouldSatisfy` \_ ->
              nearZero (respos resu - V3 0.8 2.475 0.8)
                && nearZero (resdis resu)
                  && restou resu /= NewlyTouchingGround {newonground = LT}
      it "jumps" do
        let model = stones [V3 0 0 0]
            zombie = zombiebycenter (V3 0.5 1.975 0.5)
            disp = V3 0 0.5 0
            resu = run model (resolve zombie disp)
         in resu `shouldSatisfy` \_ ->
              nearZero (respos resu - V3 0.5 2.475 0.5)
                && nearZero (resdis resu)
                  && restou resu == NewlyTouchingGround {newonground = GT}
