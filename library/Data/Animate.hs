module Data.Animate
  ( Seconds
  , DeltaSeconds
  , Frame(..)
  , KeyFrames
  , keyFrames
  , framesByKeyFrame
  , Loop(..)
  , Position(..)
  , FrameStep(..)
  , stepFrame
  , step
  ) where

import qualified Data.Vector as V (Vector, (!), length, fromList)

type Seconds = Float
type DeltaSeconds = Seconds

data Frame loc = Frame
  { _fLocation :: loc
  , _fSeconds :: Seconds
  } deriving (Show, Eq)

newtype KeyFrames kf loc = KeyFrames { unKeyFrames :: V.Vector (V.Vector (Frame loc)) }
  deriving (Show, Eq)

keyFrames :: (Enum kf, Bounded kf) => (kf -> [Frame loc]) -> KeyFrames kf loc
keyFrames getFrames = KeyFrames $ V.fromList $ map (V.fromList . getFrames) [minBound..maxBound]

framesByKeyFrame :: Enum kf => KeyFrames kf loc -> kf -> V.Vector (Frame loc)
framesByKeyFrame (KeyFrames kfs) kf = kfs V.! fromEnum kf

data Loop
  = LoopForever
  | LoopCount Int
  deriving (Show, Eq)

data Position kf = Position
  { _pKeyFrame :: kf
  , _pFrameIndex :: Int
  , _pCounter :: Seconds
  , _pLoop :: Loop
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

step :: Enum kf => KeyFrames kf loc -> Position kf -> DeltaSeconds -> Position kf
step kfs p d =
  if _fsFrameCompletion 
    then step kfs p' _fsRemainingDelta
    else p{_pCounter = _fsCounter}
  where
    FrameStep{_fsFrameCompletion, _fsCounter, _fsRemainingDelta} = stepFrame f p d
    fs = unKeyFrames kfs V.! fromEnum (_pKeyFrame p)
    f = fs V.! _pFrameIndex p
    p'= case _pLoop p of
      LoopForever -> p{_pFrameIndex = (_pFrameIndex p + 1) `mod` V.length fs, _pCounter = 0}
      LoopCount n -> let
        index = (_pFrameIndex p + 1) `mod` V.length fs
        n' = if index == 0 then n - 1 else n
        in p
          { _pFrameIndex = if n' < 0 then _pFrameIndex p else index
          , _pCounter = 0
          , _pLoop = LoopCount n' }

