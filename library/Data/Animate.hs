module Data.Animate
  ( Seconds
  , DeltaSeconds
  , Frame(..)
  , Animations
  , animations
  , framesByAnimation
  , Loop(..)
  , Position(..)
  , FrameStep(..)
  , stepFrame
  , stepAnimation
  , isAnimationComplete
  ) where

import qualified Data.Vector as V (Vector, (!), length, fromList)

-- | Avoided newtype wrapper for convenience (experimental)
type Seconds = Float

-- | Type aliased seconds (experimental)
type DeltaSeconds = Seconds

data Frame loc = Frame
  { _fLocation :: loc -- ^ User defined reference to the location of a sprite. For example, a sprite sheet clip.
  , _fDelay :: Seconds -- ^ Minimium amount of time for the frame to last.
  } deriving (Show, Eq)

-- | Type safe animation set. Use an sum type with an `Enum` and `Bounded` instance for the animation `a`.
newtype Animations a loc = Animations { unAnimations :: V.Vector (V.Vector (Frame loc)) }
  deriving (Show, Eq)

-- | Generate animations given each constructor
animations :: (Enum a, Bounded a) => (a -> [Frame loc]) -> Animations a loc
animations getFrames = Animations $ V.fromList $ map (V.fromList . getFrames) [minBound..maxBound]

-- | Lookup the frames of an animation
framesByAnimation :: Enum a => Animations a loc -> a -> V.Vector (Frame loc)
framesByAnimation (Animations as) a = as V.! fromEnum a

data Loop
  = LoopForever -- ^ Never stop looping. Animation can never be completed.
  | LoopCount Int -- ^ Count down loops to below zero. 0 = no loop. 1 = one loop. 2 = two loops. etc.
  deriving (Show, Eq)

-- | State for progression through an animation
data Position a = Position
  { _pAnimation :: a -- | Index for the animation.
  , _pFrameIndex :: Int -- | Index wihin the animation. WARNING: Modifying to below zero or equal-to-or-greater-than-the-frame-count will throw out of bounds errors.
  , _pCounter :: Seconds -- | Accumulated seconds to end of the frame. Will continue to compound if animation is completed.
  , _pLoop :: Loop -- | How to loop through an animation. LoopCount is a count down.
  } deriving (Show, Eq)

-- | You can ignore. An intermediate type for `stepAnimation` to judge how to increment the current frame.
data FrameStep
  = FrameStepCounter Seconds -- | New counter to compare against the frame's delay.
  | FrameStepDelta DeltaSeconds -- | How much delta to carry over into the next frame.
  deriving (Show, Eq)

-- | Intermediate function for how a frame should be step through.
stepFrame :: Frame loc -> Position a -> DeltaSeconds -> FrameStep
stepFrame Frame{_fDelay} Position{_pCounter} delta = 
  if _pCounter + delta >= _fDelay
    then FrameStepDelta $ _pCounter + delta - _fDelay
    else FrameStepCounter $ _pCounter + delta

-- | Step through the animation resulting in a new position.
stepAnimation :: Enum a => Animations a loc -> Position a -> DeltaSeconds -> Position a
stepAnimation as p d =
  case frameStep of
    FrameStepCounter counter -> p{_pCounter = counter }
    FrameStepDelta delta -> stepAnimation as p' delta
  where
    frameStep = stepFrame f p d
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

-- | The animation has finished all its frames. Useful for signalling into switching to another animation.
--   With a LoopForever, the animation will never be completed.
isAnimationComplete :: Enum a => Animations a loc -> Position a -> Bool
isAnimationComplete as p = case _pLoop p of
  LoopForever -> False
  LoopCount n -> n < 0 && _pFrameIndex p == lastIndex && _pCounter p >= _fDelay lastFrame
  where
    frames = framesByAnimation as (_pAnimation p)
    lastIndex = V.length frames - 1
    lastFrame = frames V.! lastIndex
