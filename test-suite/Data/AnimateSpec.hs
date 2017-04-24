{-# LANGUAGE DuplicateRecordFields #-}
module Data.AnimateSpec where

import qualified Data.Vector as V
import Test.Hspec
import Data.Animate

data Animation0 = Animation0Stand | Animation0Walk
  deriving (Show, Eq, Enum, Bounded)

spec :: Spec
spec = do
  describe "mkAnimations" $ do
    let getFrames Animation0Stand = [Frame 'a' 0.2, Frame 'b' 0.2]
        getFrames Animation0Walk = [Frame 'c' 0.2, Frame 'd' 0.2]
    let as = animations getFrames
    it "should have the correct frames for the given keyframe" $ do
      framesByAnimation as Animation0Stand `shouldBe` V.fromList [Frame 'a' 0.2, Frame 'b' 0.2]
      framesByAnimation as Animation0Walk `shouldBe` V.fromList [Frame 'c' 0.2, Frame 'd' 0.2]
  describe "stepFrame" $ do
    it "should have left over delta seconds and set the frame completion flag" $ do
      let delta = 0.9
      let actual = stepFrame Frame { _fLocation = 'a', _fDelay = 1.0 } Position { _pAnimation = (0 :: Int), _pFrameIndex = 0, _pCounter = 0.3, _pLoop = LoopForever } delta
      let expected = 0.2
      actual `shouldSatisfy` (\(FrameStepDelta actual') -> 1e6 > abs (actual' - expected))
  describe "stepAnimation" $ do
    let getFrames Animation0Stand = [Frame 'a' 0.2, Frame 'b' 0.2]
        getFrames Animation0Walk = [Frame 'c' 0.2, Frame 'd' 0.2]
    let as = animations getFrames
    let p = Position { _pAnimation = Animation0Stand, _pFrameIndex = 0, _pCounter = 0, _pLoop = LoopForever }
    it "should do nothing if given 0 delta seconds" $ do
      stepAnimation as p 0 `shouldBe` p
    it "should go to the next frame" $ do
      stepAnimation as p 0.2 `shouldBe` p { _pFrameIndex = 1, _pCounter = 0 }
    it "should loop to the start" $ do
      stepAnimation as p 0.4 `shouldBe` p { _pFrameIndex = 0, _pCounter = 0 }
    it "should loop once" $ do
      stepAnimation as p{ _pLoop = LoopCount 1 } 0.4 `shouldBe` p { _pFrameIndex = 0, _pCounter = 0, _pLoop = LoopCount 0 }
    it "should not loop" $ do
      stepAnimation as p{ _pLoop = LoopCount 0 } 0.4 `shouldBe` p { _pFrameIndex = 1, _pCounter = 0, _pLoop = LoopCount (-1) }
  describe "isAnimationComplete" $ do
    let getFrames Animation0Stand = [Frame 'a' 0.2, Frame 'b' 0.2]
        getFrames Animation0Walk = [Frame 'c' 0.2, Frame 'd' 0.2]
    let as = animations getFrames
    it "should be incomplete: loop is forever" $ do
      let p = Position { _pAnimation = Animation0Stand, _pFrameIndex = 0, _pCounter = 0, _pLoop = LoopForever }
      isAnimationComplete as p `shouldBe` False
    it "should be incomplete: frame isn't at the end and loop count is negative" $ do
      let p = Position { _pAnimation = Animation0Stand, _pFrameIndex = 0, _pCounter = 0, _pLoop = LoopCount (-1) }
      isAnimationComplete as p `shouldBe` False
    it "should be complete: frame is at the end and loop count is negative and counter gte than delay" $ do
      let p = Position { _pAnimation = Animation0Stand, _pFrameIndex = 1, _pCounter = 0.2, _pLoop = LoopCount (-1) }
      isAnimationComplete as p `shouldBe` True
    it "should be incomplete: frame is at the end and loop count is non-negative" $ do
      let p = Position { _pAnimation = Animation0Stand, _pFrameIndex = 0, _pCounter = 0, _pLoop = LoopCount 0 }
      isAnimationComplete as p `shouldBe` False
    it "should be incomplete: frame isn't at the end and loop count is non-negative" $ do
      let p = Position { _pAnimation = Animation0Stand, _pFrameIndex = 0, _pCounter = 0, _pLoop = LoopCount (-1) }
      isAnimationComplete as p `shouldBe` False
