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
  , positionHasLooped
  ) where

import qualified Data.Vector as V (Vector, (!), length, fromList)

-- | Avoided newtype wrapper for convenience (tentative)
type Seconds = Float

-- | Type aliased seconds (tentative)
type DeltaSeconds = Seconds

data Frame loc = Frame
  { frameLocation :: loc -- ^ User defined reference to the location of a sprite. For example, a sprite sheet clip.
  , frameDelay :: Seconds -- ^ Minimium amount of time for the frame to last.
  } deriving (Show, Eq)

-- | Type safe animation set. Use an sum type with an `Enum` and `Bounded` instance for the animation, 'a'.
newtype Animations a loc = Animations { unAnimations :: V.Vector (V.Vector (Frame loc)) }
  deriving (Show, Eq)

-- | Generate animations given each constructor
animations :: (Enum a, Bounded a) => (a -> [Frame loc]) -> Animations a loc
animations getFrames = Animations $ V.fromList $ map (V.fromList . getFrames) [minBound..maxBound]

-- | Lookup the frames of an animation
framesByAnimation :: Enum a => Animations a loc -> a -> V.Vector (Frame loc)
framesByAnimation (Animations as) a = as V.! fromEnum a

data Loop
  = Loop'Forever -- ^ Never stop looping. Animation can never be completed.
  | Loop'Count Int -- ^ Count down loops to below zero. 0 = no loop. 1 = one loop. 2 = two loops. etc.
  deriving (Show, Eq)

-- | State for progression through an animation
data Position a = Position
  { positionAnimation :: a -- ^ Index for the animation.
  , positionFrameIndex :: Int -- ^ Index wihin the animation. WARNING: Modifying to below zero or equal-to-or-greater-than-the-frame-count will throw out of bounds errors.
  , positionCounter :: Seconds -- ^ Accumulated seconds to end of the frame. Will continue to compound if animation is completed.
  , positionLoop :: Loop -- ^ How to loop through an animation. Loop'Count is a count down.
  } deriving (Show, Eq)

-- | You can ignore. An intermediate type for `stepAnimation` to judge how to increment the current frame.
data FrameStep
  = FrameStep'Counter Seconds -- ^ New counter to compare against the frame's delay.
  | FrameStep'Delta DeltaSeconds -- ^ How much delta to carry over into the next frame.
  deriving (Show, Eq)

-- | Intermediate function for how a frame should be step through.
stepFrame :: Frame loc -> Position a -> DeltaSeconds -> FrameStep
stepFrame Frame{frameDelay} Position{positionCounter} delta = 
  if positionCounter + delta >= frameDelay
    then FrameStep'Delta $ positionCounter + delta - frameDelay
    else FrameStep'Counter $ positionCounter + delta

-- | Step through the animation resulting in a new position.
stepAnimation :: Enum a => Animations a loc -> Position a -> DeltaSeconds -> Position a
stepAnimation as p d =
  case frameStep of
    FrameStep'Counter counter -> p{positionCounter = counter }
    FrameStep'Delta delta -> stepAnimation as p' delta
  where
    frameStep = stepFrame f p d
    fs = unAnimations as V.! fromEnum (positionAnimation p)
    f = fs V.! positionFrameIndex p
    p'= case positionLoop p of
      Loop'Forever -> p{positionFrameIndex = (positionFrameIndex p + 1) `mod` V.length fs, positionCounter = 0}
      Loop'Count n -> let
        index = (positionFrameIndex p + 1) `mod` V.length fs
        n' = if index == 0 then n - 1 else n
        in p
          { positionFrameIndex = if n' < 0 then positionFrameIndex p else index
          , positionCounter = 0
          , positionLoop = Loop'Count n' }

-- | The animation has finished all its frames. Useful for signalling into switching to another animation.
--   With a Loop'Forever, the animation will never be completed.
isAnimationComplete :: Enum a => Animations a loc -> Position a -> Bool
isAnimationComplete as p = case positionLoop p of
  Loop'Forever -> False
  Loop'Count n -> n < 0 && positionFrameIndex p == lastIndex && positionCounter p >= frameDelay lastFrame
  where
    frames = framesByAnimation as (positionAnimation p)
    lastIndex = V.length frames - 1
    lastFrame = frames V.! lastIndex


-- | Simple function diff'ing the position for loop change (tentative)
positionHasLooped
  :: Position a -- ^ Previous
  -> Position a -- ^ Next
  -> Bool
positionHasLooped Position{ positionLoop = Loop'Count c } Position{ positionLoop = Loop'Count c' } = c > c'
positionHasLooped Position{ positionLoop = Loop'Forever } _ = False
positionHasLooped _ Position{ positionLoop = Loop'Forever } = False
