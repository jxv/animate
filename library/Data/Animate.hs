module Data.Animate
  ( Seconds
  , DeltaSeconds
  , Frame(..)

  , KeyFrames
  , keyFrames
  , framesByKeyFrame

  , Position(..)
  , FrameStep(..)
  , stepFrame

  , Loop(..)
  ) where

import qualified Data.Vector as V (Vector, (!), length, fromList)

type Seconds = Float
type DeltaSeconds = Seconds

data Frame loc = Frame
  { _fLocation :: loc
  , _fSeconds :: Seconds
  } deriving (Show, Eq)

newtype KeyFrames kf loc = KeyFrames (V.Vector (V.Vector (Frame loc)))
  deriving (Show, Eq)

keyFrames :: (Enum kf, Bounded kf) => (kf -> [Frame loc]) -> KeyFrames kf loc
keyFrames getFrames = KeyFrames $ V.fromList $ map (V.fromList . getFrames) [minBound..maxBound]

framesByKeyFrame :: Enum kf => KeyFrames kf loc -> kf -> V.Vector (Frame loc)
framesByKeyFrame (KeyFrames kfs) kf = kfs V.! fromEnum kf

data Position kf = Position
  { _pKeyFrame :: kf
  , _pFrameIndex :: Int
  , _pCounter :: Seconds
  } deriving (Show, Eq)

data FrameStep = FrameStep
  { _fsFrameCompletion :: Bool
  , _fsCounter :: Seconds
  , _fsRemainingDelta :: DeltaSeconds
  } deriving (Show, Eq)

stepFrame :: Frame loc -> Position kf -> DeltaSeconds -> FrameStep
stepFrame Frame{_fSeconds} Position{_pCounter} delta = let
  completion = _pCounter + delta >= _fSeconds
  counter = if completion then 0 else _pCounter + delta
  delta' = if completion then _pCounter + delta - _fSeconds else 0
  in FrameStep completion counter delta'

data Loop
  = LoopForever
  | LoopCount Int
  deriving (Show, Eq)
