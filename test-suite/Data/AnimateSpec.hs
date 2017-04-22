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
