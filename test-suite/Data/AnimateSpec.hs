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
    let as = keyFrames getFrames
    it "should have the correct frames for the given keyframe" $ do
      framesByAnimation as Animation0Stand `shouldBe` V.fromList [Frame 'a' 0.2, Frame 'b' 0.2]
      framesByAnimation as Animation0Walk `shouldBe` V.fromList [Frame 'c' 0.2, Frame 'd' 0.2]
  describe "stepFrame" $ do
    it "should have left over delta seconds and set the frame completion flag" $ do
      let delta = 0.9
      let actual = stepFrame Frame { _fLocation = 'a', _fSeconds = 1.0 } Position { _pAnimation = (0 :: Int), _pFrameIndex = 0, _pCounter = 0.3, _pLoop = LoopForever } delta
      let expected = FrameStep { _fsFrameCompletion = True, _fsCounter = 0.0, _fsRemainingDelta = 0.2 }
      _fsFrameCompletion actual `shouldBe` _fsFrameCompletion expected
      _fsCounter actual `shouldBe` _fsCounter expected
      _fsRemainingDelta actual `shouldSatisfy` (\rd -> 1e6 > abs (rd - _fsRemainingDelta expected))
  describe "stepAnimation" $ do
    let getFrames Animation0Stand = [Frame 'a' 0.2, Frame 'b' 0.2]
        getFrames Animation0Walk = [Frame 'c' 0.2, Frame 'd' 0.2]
    let as = keyFrames getFrames
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
