{-# LANGUAGE DuplicateRecordFields #-}
module Data.AnimateSpec where

import qualified Data.Vector as V
import Test.Hspec
import Data.Animate

data KeyFrame0 = KeyFrame0Stand | KeyFrame0Walk
  deriving (Show, Eq, Enum, Bounded)

spec :: Spec
spec = do
  describe "mkKeyFrames" $ do
    let getFrames KeyFrame0Stand = [Frame 'a' 0.2, Frame 'b' 0.2]
        getFrames KeyFrame0Walk = [Frame 'c' 0.2, Frame 'd' 0.2]
    let kfs = keyFrames getFrames
    it "should have the correct frames for the given keyframe" $ do
      framesByKeyFrame kfs KeyFrame0Stand `shouldBe` V.fromList [Frame 'a' 0.2, Frame 'b' 0.2]
      framesByKeyFrame kfs KeyFrame0Walk `shouldBe` V.fromList [Frame 'c' 0.2, Frame 'd' 0.2]
  describe "stepFrame" $ do
    it "should have left over delta seconds and set the frame completion flag" $ do
      let delta = 0.9
      let actual = stepFrame Frame { _fLocation = 'a', _fSeconds = 1.0 } Position { _pKeyFrame = (0 :: Int), _pFrameIndex = 0, _pCounter = 0.3, _pLoop = LoopForever } delta
      let expected = FrameStep { _fsFrameCompletion = True, _fsCounter = 0.0, _fsRemainingDelta = 0.2 }
      _fsFrameCompletion actual `shouldBe` _fsFrameCompletion expected
      _fsCounter actual `shouldBe` _fsCounter expected
      _fsRemainingDelta actual `shouldSatisfy` (\rd -> 1e6 > abs (rd - _fsRemainingDelta expected))
  describe "step" $ do
    let getFrames KeyFrame0Stand = [Frame 'a' 0.2, Frame 'b' 0.2]
        getFrames KeyFrame0Walk = [Frame 'c' 0.2, Frame 'd' 0.2]
    let kfs = keyFrames getFrames
    let p = Position { _pKeyFrame = KeyFrame0Stand, _pFrameIndex = 0, _pCounter = 0, _pLoop = LoopForever }
    it "should do nothing if given 0 delta seconds" $ do
      step kfs p 0 `shouldBe` p
    it "should go to the next frame" $ do
      step kfs p 0.2 `shouldBe` p { _pFrameIndex = 1, _pCounter = 0 }
    it "should loop to the start" $ do
      step kfs p 0.4 `shouldBe` p { _pFrameIndex = 0, _pCounter = 0 }
    it "should loop once" $ do
      step kfs p{ _pLoop = LoopCount 1 } 0.4 `shouldBe` p { _pFrameIndex = 0, _pCounter = 0, _pLoop = LoopCount 0 }
    it "should not loop" $ do
      step kfs p{ _pLoop = LoopCount 0 } 0.4 `shouldBe` p { _pFrameIndex = 1, _pCounter = 0, _pLoop = LoopCount (-1) }

