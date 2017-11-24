module Main where

import qualified SDL
import qualified SDL.Image as Image
import qualified Data.Animate as Ani

import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import Data.StateVar (($=))
import Foreign.C.Types
import SDL.Vect

data DinoKey
  = DinoKey'Idle
  | DinoKey'Move
  | DinoKey'Kick
  | DinoKey'Hurt
  | DinoKey'Sneak
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Ani.Key DinoKey
instance Ani.KeyName DinoKey where
  keyName = \case
    DinoKey'Idle -> "Idle"
    DinoKey'Move -> "Move"
    DinoKey'Kick -> "Kick"
    DinoKey'Hurt -> "Hurt"
    DinoKey'Sneak -> "Sneak"

loadSurface :: FilePath -> Maybe Ani.Color -> IO SDL.Surface
loadSurface path alpha = do
  surface <- Image.load path
  case alpha of
    Just (r,g,b) -> SDL.surfaceColorKey surface $= (Just $ V4 r g b 0x00)
    Nothing -> return ()
  return surface

main :: IO ()
main = do
  putStrLn "Press Space to iterate through animation keys"
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Animate Example" SDL.defaultWindow { SDL.windowInitialSize = V2 320 180 }
  SDL.showWindow window
  screen <- SDL.getWindowSurface window
  spriteSheet <- Ani.readSpriteSheetJSON loadSurface "dino.json" :: IO (Ani.SpriteSheet DinoKey SDL.Surface)
  loop window screen spriteSheet (Ani.initPosition DinoKey'Idle)
  SDL.destroyWindow window
  SDL.quit

detectSpacePressed :: SDL.EventPayload -> Bool
detectSpacePressed event = case event of
  SDL.KeyboardEvent SDL.KeyboardEventData{keyboardEventKeysym = SDL.Keysym{keysymKeycode = code}, keyboardEventKeyMotion = motion, keyboardEventRepeat = repeated} ->
    code == SDL.KeycodeSpace &&
    motion == SDL.Pressed &&
    not repeated
  _ -> False

loop :: SDL.Window -> SDL.Surface -> Ani.SpriteSheet DinoKey SDL.Surface -> Ani.Position DinoKey -> IO ()
loop window screen ss@Ani.SpriteSheet{ssAnimations, ssImage} pos = do
  events <- map SDL.eventPayload <$> SDL.pollEvents
  let quit = elem SDL.QuitEvent events
  let toNextKey = any detectSpacePressed events
  let pos' = Ani.stepPosition ssAnimations pos frameDeltaSeconds
  let loc = Ani.currentLocation ssAnimations pos'
  SDL.surfaceFillRect screen Nothing (V4 0 0 0 0) -- Clear screen
  _ <- SDL.surfaceBlit ssImage (Just $ rectFromClip loc) screen Nothing
  SDL.updateWindowSurface window
  delayMilliseconds frameDeltaMilliseconds
  let pos'' = if toNextKey then Ani.initPosition (Ani.nextKey (Ani.pKey pos')) else pos'
  unless quit $ loop window screen ss pos''
  where
    frameDeltaSeconds = 0.016667
    frameDeltaMilliseconds = 16

rectFromClip :: Ani.SpriteClip -> SDL.Rectangle CInt
rectFromClip Ani.SpriteClip{scX,scY,scW,scH} = SDL.Rectangle (SDL.P (V2 (num scX) (num scY))) (V2 (num scW) (num scH))
  where
    num = fromIntegral

delayMilliseconds :: Int -> IO ()
delayMilliseconds ms = threadDelay (10000 * ms)
