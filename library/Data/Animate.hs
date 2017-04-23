module Data.Animate
  ( Seconds
  , DeltaSeconds
  , Frame(..)
  , Animations
  , keyFrames
  , framesByAnimation
  , Loop(..)
  , Position(..)
  , FrameStep(..)
  , stepFrame
  , stepAnimation
  ) where

import qualified Data.Vector as V (Vector, (!), length, fromList)

type Seconds = Float
type DeltaSeconds = Seconds

data Frame loc = Frame
  { _fLocation :: loc
  , _fSeconds :: Seconds
  } deriving (Show, Eq)

newtype Animations a loc = Animations { unAnimations :: V.Vector (V.Vector (Frame loc)) }
  deriving (Show, Eq)

keyFrames :: (Enum a, Bounded a) => (a -> [Frame loc]) -> Animations a loc
keyFrames getFrames = Animations $ V.fromList $ map (V.fromList . getFrames) [minBound..maxBound]

framesByAnimation :: Enum a => Animations a loc -> a -> V.Vector (Frame loc)
framesByAnimation (Animations as) a = as V.! fromEnum a

data Loop
  = LoopForever
  | LoopCount Int
  deriving (Show, Eq)

data Position a = Position
  { _pAnimation :: a
  , _pFrameIndex :: Int
  , _pCounter :: Seconds
  , _pLoop :: Loop
  } deriving (Show, Eq)

data FrameStep = FrameStep
  { _fsFrameCompletion :: Bool
  , _fsCounter :: Seconds
  , _fsRemainingDelta :: DeltaSeconds
  } deriving (Show, Eq)

stepFrame :: Frame loc -> Position a -> DeltaSeconds -> FrameStep
stepFrame Frame{_fSeconds} Position{_pCounter} delta = let
  completion = _pCounter + delta >= _fSeconds
  counter = if completion then 0 else _pCounter + delta
  delta' = if completion then _pCounter + delta - _fSeconds else 0
  in FrameStep completion counter delta'

stepAnimation :: Enum a => Animations a loc -> Position a -> DeltaSeconds -> Position a
stepAnimation as p d =
  if _fsFrameCompletion 
    then stepAnimation as p' _fsRemainingDelta
    else p{_pCounter = _fsCounter}
  where
    FrameStep{_fsFrameCompletion, _fsCounter, _fsRemainingDelta} = stepFrame f p d
    fs = unAnimations as V.! fromEnum (_pAnimation p)
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
