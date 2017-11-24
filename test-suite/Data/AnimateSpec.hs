{-# LANGUAGE LambdaCase #-}
module Data.AnimateSpec where

import qualified Data.Vector as V
import Test.Hspec
import Data.Animate

data Ani = Ani'Stand | Ani'Walk
  deriving (Show, Eq, Enum, Ord, Bounded)

instance Key Ani

spec :: Spec
spec = do
  describe "mkAnimations" $ do
    let getFrames Ani'Stand = [Frame 'a' 0.2, Frame 'b' 0.2]
        getFrames Ani'Walk = [Frame 'c' 0.2, Frame 'd' 0.2]
    let as = animations getFrames
    it "should have the correct frames for the given keyframe" $ do
      framesByAnimation as Ani'Stand `shouldBe` V.fromList [Frame 'a' 0.2, Frame 'b' 0.2]
      framesByAnimation as Ani'Walk `shouldBe` V.fromList [Frame 'c' 0.2, Frame 'd' 0.2]

  describe "stepFrame" $ do
    it "should have left over delta seconds and set the frame completion flag" $ do
      let delta = 0.9
      let actual = stepFrame Frame { fLocation = 'a', fDelay = 1.0 } Position { pKey = (0 :: Int), pFrameIndex = 0, pCounter = 0.3, pLoop = Loop'Always } delta
      let expected = 0.2
      actual `shouldSatisfy` (\(FrameStep'Delta actual') -> 1e6 > abs (actual' - expected))

  describe "stepPosition" $ do
    let getFrames Ani'Stand = [Frame 'a' 0.2, Frame 'b' 0.2]
        getFrames Ani'Walk = [Frame 'c' 0.2, Frame 'd' 0.2]
    let as = animations getFrames
    let p = Position { pKey = Ani'Stand, pFrameIndex = 0, pCounter = 0, pLoop = Loop'Always }
    it "should do nothing if given 0 delta seconds" $ do
      stepPosition as p 0 `shouldBe` p
    it "should go to the next frame" $ do
      stepPosition as p 0.2 `shouldBe` p { pFrameIndex = 1, pCounter = 0 }
    it "should loop to the start" $ do
      stepPosition as p 0.4 `shouldBe` p { pFrameIndex = 0, pCounter = 0 }
    it "should loop once" $ do
      stepPosition as p{ pLoop = Loop'Count 1 } 0.4 `shouldBe` p { pFrameIndex = 0, pCounter = 0, pLoop = Loop'Count 0 }
    it "should not loop" $ do
      stepPosition as p{ pLoop = Loop'Count 0 } 0.4 `shouldBe` p { pFrameIndex = 1, pCounter = 0, pLoop = Loop'Count (-1) }

  describe "isAnimationComplete" $ do
    let getFrames Ani'Stand = [Frame 'a' 0.2, Frame 'b' 0.2]
        getFrames Ani'Walk = [Frame 'c' 0.2, Frame 'd' 0.2]
    let as = animations getFrames
    it "should be incomplete: loop is forever" $ do
      let p = Position { pKey = Ani'Stand, pFrameIndex = 0, pCounter = 0, pLoop = Loop'Always }
      isAnimationComplete as p `shouldBe` False
    it "should be incomplete: frame isn't at the end and loop count is negative" $ do
      let p = Position { pKey = Ani'Stand, pFrameIndex = 0, pCounter = 0, pLoop = Loop'Count (-1) }
      isAnimationComplete as p `shouldBe` False
    it "should be complete: frame is at the end and loop count is negative and counter gte than delay" $ do
      let p = Position { pKey = Ani'Stand, pFrameIndex = 1, pCounter = 0.2, pLoop = Loop'Count (-1) }
      isAnimationComplete as p `shouldBe` True
    it "should be incomplete: frame is at the end and loop count is non-negative" $ do
      let p = Position { pKey = Ani'Stand, pFrameIndex = 0, pCounter = 0, pLoop = Loop'Count 0 }
      isAnimationComplete as p `shouldBe` False
    it "should be incomplete: frame isn't at the end and loop count is non-negative" $ do
      let p = Position { pKey = Ani'Stand, pFrameIndex = 0, pCounter = 0, pLoop = Loop'Count (-1) }
      isAnimationComplete as p `shouldBe` False

  describe "positionHasLooped" $ do
    it "should have looped" $ do
      let p = Position { pKey = Ani'Stand, pFrameIndex = 0, pCounter = 0, pLoop = Loop'Count 0 }
      let p' = Position { pKey = Ani'Stand, pFrameIndex = 0, pCounter = 0, pLoop = Loop'Count (-1) }
      positionHasLooped p p' `shouldBe` True
    it "should not have looped" $ do
      let p = Position { pKey = Ani'Stand, pFrameIndex = 0, pCounter = 0, pLoop = Loop'Count 0 }
      let p' = Position { pKey = Ani'Stand, pFrameIndex = 0, pCounter = 0, pLoop = Loop'Count 0 }
      positionHasLooped p p' `shouldBe` False
