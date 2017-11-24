module Data.Animate
  ( Seconds
  , DeltaSeconds
  , Color
  , FrameIndex
  , Frame(..)
  , Animations
  , Loop(..)
  , Position(..)
  , FrameStep(..)
  , Key
  , KeyName
  , SpriteClip(..)
  , SpriteSheet(..)
  , SpriteSheetInfo(..)
  , animations
  , framesByAnimation
  , initPosition
  , initPositionLoops
  , initPositionWithLoop
  , stepFrame
  , stepAnimation
  , isAnimationComplete
  , positionHasLooped
  , currentFrame
  , currentLocation
  , readSpriteSheetInfoJSON
  , readSpriteSheetJSON
  ) where

import qualified Data.Vector as V (Vector, (!), length, fromList)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), decode, object, (.=), Value(..))
import Data.Map (Map)
import Data.Word (Word8)
import Data.Text (Text)

-- | Avoided newtype wrapper for convenience (tentative)
type Seconds = Float

-- | Type aliased seconds (tentative)
type DeltaSeconds = Seconds

-- | Alias for RGB (8bit, 8bit, 8bit)
type Color = (Word8, Word8, Word8)

type FrameIndex = Int

data Frame loc = Frame
  { fLocation :: loc -- ^ User defined reference to the location of a sprite. For example, a sprite sheet clip.
  , fDelay :: Seconds -- ^ Minimium amount of time for the frame to last.
  } deriving (Show, Eq)

-- | Type safe animation set. Use an sum type with an `Enum` and `Bounded` instance for the animation, 'a'.
newtype Animations key loc = Animations { unAnimations :: V.Vector (V.Vector (Frame loc)) }
  deriving (Show, Eq)

-- | Sematically for an animation key constraint
class (Ord key, Bounded key, Enum key) => Key key

-- | Animation Keyframe. `keyName` is used for JSON parsing.
class Key key => KeyName key where
  keyName :: key -> Text

-- | Describe the boxed area of the 2d sprite inside a sprite sheet
data SpriteClip = SpriteClip
  { scX :: Int
  , scY :: Int
  , scW :: Int
  , scH :: Int
  } deriving (Show, Eq)

instance ToJSON SpriteClip where
  toJSON SpriteClip{scX,scY,scW,scH} = toJSON (scX, scY, scW, scH)

instance FromJSON SpriteClip where
  parseJSON v = do
    (x,y,w,h) <- parseJSON v
    return SpriteClip { scX = x, scY = y, scW = w, scH = h }

-- | Generalized sprite sheet data structure
data SpriteSheet key img = SpriteSheet
  { ssAnimations :: Animations key SpriteClip
  , ssImage :: img
  }

-- | One way to represent sprite sheet information.
-- | JSON loading is included.
data SpriteSheetInfo = SpriteSheetInfo
  { ssiImage :: FilePath
  , ssiAlpha :: Maybe Color
  , ssiClips :: [SpriteClip]
  , ssiAnimations :: Map Text [(FrameIndex, Seconds)]
  } deriving (Show, Eq)

instance ToJSON SpriteSheetInfo where
  toJSON SpriteSheetInfo{ssiImage,ssiAlpha,ssiClips,ssiAnimations} = object
    [ "image" .= ssiImage
    , "alpha" .= ssiAlpha
    , "clips" .= ssiClips
    , "animations" .= ssiAnimations
    ]

instance FromJSON SpriteSheetInfo where
  parseJSON (Object o) = do
    image <- o .: "image"
    alpha <- o .: "alpha"
    clips <- o .: "clips"
    anis <- o .: "animations"
    return SpriteSheetInfo { ssiImage = image, ssiAlpha = alpha, ssiClips = clips, ssiAnimations = anis }
  parseJSON _ = mzero

-- | Generate animations given each constructor
animations :: Key key => (key -> [Frame loc]) -> Animations key loc
animations getFrames = Animations $ V.fromList $ map (V.fromList . getFrames) [minBound..maxBound]

-- | Lookup the frames of an animation
framesByAnimation :: Key key => Animations key loc -> key -> V.Vector (Frame loc)
framesByAnimation (Animations as) k = as V.! fromEnum k

data Loop
  = Loop'Always -- ^ Never stop looping. Animation can never be completed.
  | Loop'Count Int -- ^ Count down loops to below zero. 0 = no loop. 1 = one loop. 2 = two loops. etc.
  deriving (Show, Eq)

-- | State for progression through an animation
-- | `example = Position minBound 0 0 Loop'Always`
data Position key = Position
  { pKey :: key -- ^ Index for the animation.
  , pFrameIndex :: FrameIndex -- ^ Index wihin the animation. WARNING: Modifying to below zero or equal-to-or-greater-than-the-frame-count will throw out of bounds errors.
  , pCounter :: Seconds -- ^ Accumulated seconds to end of the frame. Will continue to compound if animation is completed.
  , pLoop :: Loop -- ^ How to loop through an animation. Loop'Count is a count down.
  } deriving (Show, Eq)

-- | New `Position` with its animation key to loop forever
initPosition :: Key key => key -> Position key
initPosition key = initPositionWithLoop key Loop'Always

-- | New `Position` with its animation key with a limited loop
initPositionLoops :: Key key => key -> Int -> Position key
initPositionLoops key count = initPositionWithLoop key (Loop'Count count)

-- | New `Position`
initPositionWithLoop :: Key key => key -> Loop -> Position key
initPositionWithLoop key loop = Position
  { pKey = key
  , pFrameIndex = 0
  , pCounter = 0
  , pLoop = loop
  }

-- | You can ignore. An intermediate type for `stepAnimation` to judge how to increment the current frame.
data FrameStep
  = FrameStep'Counter Seconds -- ^ New counter to compare against the frame's delay.
  | FrameStep'Delta DeltaSeconds -- ^ How much delta to carry over into the next frame.
  deriving (Show, Eq)

-- | Intermediate function for how a frame should be step through.
stepFrame :: Frame loc -> Position key -> DeltaSeconds -> FrameStep
stepFrame Frame{fDelay} Position{pCounter} delta =
  if pCounter + delta >= fDelay
    then FrameStep'Delta $ pCounter + delta - fDelay
    else FrameStep'Counter $ pCounter + delta

-- | Step through the animation resulting in a new position.
stepAnimation :: Key key => Animations key loc -> Position key -> DeltaSeconds -> Position key
stepAnimation as p d =
  case frameStep of
    FrameStep'Counter counter -> p{pCounter = counter }
    FrameStep'Delta delta -> stepAnimation as p' delta
  where
    frameStep = stepFrame f p d
    fs = unAnimations as V.! fromEnum (pKey p)
    f = fs V.! pFrameIndex p
    p'= case pLoop p of
      Loop'Always -> p{pFrameIndex = (pFrameIndex p + 1) `mod` V.length fs, pCounter = 0}
      Loop'Count n -> let
        index = (pFrameIndex p + 1) `mod` V.length fs
        n' = if index == 0 then n - 1 else n
        in p
          { pFrameIndex = if n' < 0 then pFrameIndex p else index
          , pCounter = 0
          , pLoop = Loop'Count n' }

-- | Use the position to find the current frame of the animation.
currentFrame :: Key key => Animations key loc -> Position key -> Frame loc
currentFrame anis Position{pKey,pFrameIndex} = (framesByAnimation anis pKey) V.! pFrameIndex

-- | Use the position to find the current location, lik a sprite sheet clip, of the animation.
currentLocation :: Key key => Animations key loc -> Position key -> loc
currentLocation anis p = fLocation (currentFrame anis p)

-- | The animation has finished all its frames. Useful for signalling into switching to another animation.
--   With a Loop'Always, the animation will never be completed.
isAnimationComplete :: Key key => Animations key loc -> Position key -> Bool
isAnimationComplete as p = case pLoop p of
  Loop'Always -> False
  Loop'Count n -> n < 0 && pFrameIndex p == lastIndex && pCounter p >= fDelay lastFrame
  where
    frames = framesByAnimation as (pKey p)
    lastIndex = V.length frames - 1
    lastFrame = frames V.! lastIndex


-- | Simple function diff'ing the position for loop change (tentative)
positionHasLooped
  :: Position key -- ^ Previous
  -> Position a -- ^ Next
  -> Bool
positionHasLooped Position{ pLoop = Loop'Count c } Position{ pLoop = Loop'Count c' } = c > c'
positionHasLooped Position{ pLoop = Loop'Always } _ = False
positionHasLooped _ Position{ pLoop = Loop'Always } = False

-- | Quick function for loading `SpriteSheetInfo`.
-- | Check the example.
readSpriteSheetInfoJSON
  :: FilePath -- ^ Path of the sprite sheet info JSON file
  -> IO SpriteSheetInfo
readSpriteSheetInfoJSON path = do
  metaBytes <- BL.readFile path
  case decode metaBytes of
    Nothing -> error $ "Cannot parse Sprite Sheet Info: " ++ path
    Just ssi -> return ssi

-- | Quick function for loading `SpriteSheetInfo`, then using it to load its image for a `SpriteSheet`
-- | Check the example.
readSpriteSheetJSON
  :: KeyName key
  => (FilePath -> Maybe Color -> IO img) -- ^ Inject animage loading function
  -> FilePath -- ^ Path of the sprite sheet info JSON file
  -> IO (SpriteSheet key img)
readSpriteSheetJSON loadImage infoPath = do
  SpriteSheetInfo{ssiImage, ssiClips, ssiAnimations, ssiAlpha} <- readSpriteSheetInfoJSON infoPath
  i <- loadImage ssiImage ssiAlpha
  let frame key = (key, map (\a -> Frame (ssiClips !! fst a) (snd a)) (ssiAnimations Map.! keyName key))
  let animationMap = Map.fromList $ map frame [minBound..maxBound]
  return $ SpriteSheet (animations $ (Map.!) animationMap) i
