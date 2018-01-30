{-# LANGUAGE DeriveGeneric #-}
module Animate
  ( Color
  , FrameIndex
  , Frame(..)
  , Animations(..)
  , Loop(..)
  , Position(..)
  , FrameStep(..)
  , Key
  , KeyName(..)
  , SpriteClip(..)
  , SpriteSheet(..)
  , SpriteSheetInfo(..)
  , animations
  , framesByAnimation
  , initPosition
  , initPositionLoops
  , initPositionWithLoop
  , stepFrame
  , stepPosition
  , isAnimationComplete
  , positionHasLooped
  , currentFrame
  , currentLocation
  , nextKey
  , prevKey
  , readSpriteSheetInfoJSON
  , readSpriteSheetJSON
  ) where

import qualified Data.Vector as V (Vector, (!), length, fromList)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), eitherDecode, object, (.=), Value(..))
import Data.Map (Map)
import Data.Word (Word8)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Alias for RGB (8bit, 8bit, 8bit)
type Color = (Word8, Word8, Word8)

type FrameIndex = Int

data Frame loc delay = Frame
  { fLocation :: loc -- ^ User defined reference to the location of a sprite. For example, a sprite sheet clip.
  , fDelay :: delay -- ^ Minimium amount of units for the frame to last.
  } deriving (Show, Eq, Generic)

-- | Type safe animation set. Use a sum type with an `Enum` and `Bounded` instance for the animation, @a@.
newtype Animations key loc delay = Animations { unAnimations :: V.Vector (V.Vector (Frame loc delay)) }
  deriving (Show, Eq)

-- | Semantically for an animation key constraint
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
  , scOffset :: Maybe (Int, Int)
  } deriving (Show, Eq, Generic)

instance ToJSON SpriteClip where
  toJSON SpriteClip{scX,scY,scW,scH,scOffset} = case scOffset of
    Nothing -> toJSON (scX, scY, scW, scH)
    Just (ofsX, ofsY) -> toJSON (scX, scY, scW, scH, ofsX, ofsY)

instance FromJSON SpriteClip where
  parseJSON v =
    (do
      (x,y,w,h) <- parseJSON v
      return SpriteClip { scX = x, scY = y, scW = w, scH = h, scOffset = Nothing })
    <|>
    (do
      (x,y,w,h,ofsX,ofsY) <- parseJSON v
      return SpriteClip { scX = x, scY = y, scW = w, scH = h, scOffset = Just (ofsX, ofsY) })

-- | Generalized sprite sheet data structure
data SpriteSheet key img delay = SpriteSheet
  { ssAnimations :: Animations key SpriteClip delay
  , ssImage :: img
  } deriving (Generic)

-- | One way to represent sprite sheet information.
--   JSON loading is included.
data SpriteSheetInfo delay = SpriteSheetInfo
  { ssiImage :: FilePath
  , ssiAlpha :: Maybe Color
  , ssiClips :: [SpriteClip]
  , ssiAnimations :: Map Text [(FrameIndex, delay)]
  } deriving (Show, Eq, Generic)

instance ToJSON delay => ToJSON (SpriteSheetInfo delay) where
  toJSON SpriteSheetInfo{ssiImage,ssiAlpha,ssiClips,ssiAnimations} = object
    [ "image" .= ssiImage
    , "alpha" .= ssiAlpha
    , "clips" .= ssiClips
    , "animations" .= ssiAnimations
    ]

instance FromJSON delay => FromJSON (SpriteSheetInfo delay) where
  parseJSON (Object o) = do
    image <- o .: "image"
    alpha <- o .: "alpha"
    clips <- o .: "clips"
    anis <- o .: "animations"
    return SpriteSheetInfo { ssiImage = image, ssiAlpha = alpha, ssiClips = clips, ssiAnimations = anis }
  parseJSON _ = mzero

-- | Generate animations given each constructor
animations :: Key key => (key -> [Frame loc delay]) -> Animations key loc delay
animations getFrames = Animations $ V.fromList $ map (V.fromList . getFrames) [minBound..maxBound]

-- | Lookup the frames of an animation
framesByAnimation :: Key key => Animations key loc delay -> key -> V.Vector (Frame loc delay)
framesByAnimation (Animations as) k = as V.! fromEnum k

data Loop
  = Loop'Always -- ^ Never stop looping. Animation can never be completed.
  | Loop'Count Int -- ^ Count down loops to below zero. 0 = no loop. 1 = one loop. 2 = two loops. etc.
  deriving (Show, Eq, Generic)

-- | State for progression through an animation
--
-- > example = Position minBound 0 0 Loop'Always
data Position key delay = Position
  { pKey :: key -- ^ Index for the animation.
  , pFrameIndex :: FrameIndex -- ^ Index wihin the animation. WARNING: Modifying to below zero or equal-to-or-greater-than-the-frame-count will throw out of bounds errors.
  , pCounter :: delay -- ^ Accumulated units to end of the frame. Will continue to compound if animation is completed.
  , pLoop :: Loop -- ^ How to loop through an animation. Loop'Count is a count down.
  } deriving (Show, Eq, Generic)

-- | New `Position` with its animation key to loop forever
initPosition :: (Num delay, Key key) => key -> Position key delay
initPosition key = initPositionWithLoop key Loop'Always

-- | New `Position` with its animation key with a limited loop
initPositionLoops :: (Num delay, Key key) => key -> Int -> Position key delay
initPositionLoops key count = initPositionWithLoop key (Loop'Count count)

-- | New `Position`
initPositionWithLoop :: (Num delay, Key key) => key -> Loop -> Position key delay
initPositionWithLoop key loop = Position
  { pKey = key
  , pFrameIndex = 0
  , pCounter = 0
  , pLoop = loop
  }

-- | You can ignore. An intermediate type for `stepPosition` to judge how to increment the current frame.
data FrameStep delay
  = FrameStep'Counter delay -- ^ New counter to compare against the frame's delay.
  | FrameStep'Delta delay -- ^ How much delta to carry over into the next frame.
  deriving (Show, Eq, Generic)

-- | Intermediate function for how a frame should be step through.
stepFrame :: (Num delay, Ord delay) => Frame loc delay -> Position key delay -> delay -> FrameStep delay
stepFrame Frame{fDelay} Position{pCounter} delta =
  if pCounter + delta >= fDelay
    then FrameStep'Delta $ pCounter + delta - fDelay
    else FrameStep'Counter $ pCounter + delta

-- | Step through the animation resulting a new position.
stepPosition :: (Num delay, Ord delay, Key key) => Animations key loc delay -> Position key delay -> delay -> Position key delay
stepPosition as p d =
  case frameStep of
    FrameStep'Counter counter -> p{pCounter = counter }
    FrameStep'Delta delta -> stepPosition as p' delta
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
currentFrame :: (Num delay, Key key) => Animations key loc delay -> Position key delay -> Frame loc delay
currentFrame anis Position{pKey,pFrameIndex} = (framesByAnimation anis pKey) V.! pFrameIndex

-- | Use the position to find the current location, lik a sprite sheet clip, of the animation.
currentLocation :: (Num delay, Key key) => Animations key loc delay -> Position key delay -> loc
currentLocation anis p = fLocation (currentFrame anis p)

-- | The animation has finished all its frames. Useful for signalling into switching to another animation.
--   With a Loop'Always, the animation will never be completed.
isAnimationComplete :: (Key key, Num delay, Ord delay) => Animations key loc delay -> Position key delay -> Bool
isAnimationComplete as p = case pLoop p of
  Loop'Always -> False
  Loop'Count n -> n < 0 && pFrameIndex p == lastIndex && pCounter p >= fDelay lastFrame
  where
    frames = framesByAnimation as (pKey p)
    lastIndex = V.length frames - 1
    lastFrame = frames V.! lastIndex

-- | Cycle through the next animation key.
nextKey :: Key key => key -> key
nextKey key = if key == maxBound then minBound else succ key

-- | Cycle through the previous animation key.
prevKey :: Key key => key -> key
prevKey key = if key == minBound then maxBound else pred key

-- | Simple function diff'ing the position for loop change.
positionHasLooped
  :: Position key delay -- ^ Previous
  -> Position key delay -- ^ Next
  -> Bool
positionHasLooped Position{ pLoop = Loop'Count c } Position{ pLoop = Loop'Count c' } = c > c'
positionHasLooped Position{ pLoop = Loop'Always } _ = False
positionHasLooped _ Position{ pLoop = Loop'Always } = False

-- | Quick function for loading `SpriteSheetInfo`.
--   Check the example.
readSpriteSheetInfoJSON
  :: FromJSON delay
  => FilePath -- ^ Path of the sprite sheet info JSON file
  -> IO (SpriteSheetInfo delay)
readSpriteSheetInfoJSON path = do
  metaBytes <- BL.readFile path
  case eitherDecode metaBytes of
    Left _err -> error $ "Cannot parse Sprite Sheet Info \"" ++ path ++ "\""
    Right ssi -> return ssi

-- | Quick function for loading `SpriteSheetInfo`, then using it to load its image for a `SpriteSheet`.
--   Check the example.
readSpriteSheetJSON
  :: (KeyName key, FromJSON delay)
  => (FilePath -> Maybe Color -> IO img) -- ^ Inject an image loading function
  -> FilePath -- ^ Path of the sprite sheet info JSON file
  -> IO (SpriteSheet key img delay)
readSpriteSheetJSON loadImage infoPath = do
  SpriteSheetInfo{ssiImage, ssiClips, ssiAnimations, ssiAlpha} <- readSpriteSheetInfoJSON infoPath
  i <- loadImage ssiImage ssiAlpha
  let frame key = (key, map (\a -> Frame (ssiClips !! fst a) (snd a)) (ssiAnimations Map.! keyName key))
  let animationMap = Map.fromList $ map frame [minBound..maxBound]
  return $ SpriteSheet (animations $ (Map.!) animationMap) i
