module Data.Animate
  ( Seconds
  , DeltaSeconds
  , Frame(..)
  
  , KeyFrames
  , keyFrames
  , framesByKeyFrame

  , Position
  , Loop(..)
  ) where

import qualified Data.Vector as V (Vector, (!), length, fromList)

type Seconds = Float
type DeltaSeconds = Seconds

data Frame loc = Frame
  { _location :: loc
  , _seconds :: Seconds
  } deriving (Show, Eq)

newtype KeyFrames kf loc = KeyFrames (V.Vector (V.Vector (Frame loc)))
  deriving (Show, Eq)

keyFrames :: (Enum kf, Bounded kf) => (kf -> [Frame loc]) -> KeyFrames kf loc
keyFrames getFrames = KeyFrames $ V.fromList $ map (V.fromList . getFrames) [minBound..maxBound]

framesByKeyFrame :: (Enum kf, Bounded kf) => KeyFrames kf loc -> kf -> V.Vector (Frame loc)
framesByKeyFrame (KeyFrames kfs) kf = kfs V.! fromEnum kf

data Position kf = Position
  { _keyFrame :: kf
  , _frameIndex :: Int
  , _counter :: Seconds
  } deriving (Show, Eq)

data Loop
  = Loop
  | NoLoop
  deriving (Show, Eq)
