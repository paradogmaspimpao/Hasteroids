module Hasteroids.Keyboard ( -- Reverted
    Keyboard,
    HKey(..),
    HKeyState(..),
    initKeyboard,
    handleKeyEvent,
    isKeyDown,
    mapGLFWKeyToHasteroidsKey,
    mapGLFWKeyStateToHasteroidsKeyState
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW

data HKey = HKeySpace
          | HKeyLeft
          | HKeyRight
          | HKeyUp
          | HKeyDown
          | HKeyEsc
          | HKeyUnknown
          deriving (Eq, Ord, Show)

data HKeyState = HKeyUpState
               | HKeyDownState
               deriving (Eq, Show)

newtype Keyboard = Keyboard (Set HKey)

handleKeyEvent :: HKey -> HKeyState -> Keyboard -> Keyboard
handleKeyEvent key keyState (Keyboard s) = case keyState of
        HKeyUpState   -> Keyboard $ Set.delete key s
        HKeyDownState -> Keyboard $ Set.insert key s

initKeyboard :: Keyboard
initKeyboard = Keyboard Set.empty

isKeyDown :: Keyboard -> HKey -> Bool
isKeyDown (Keyboard s) key = Set.member key s

mapGLFWKeyToHasteroidsKey :: GLFW.Key -> HKey
mapGLFWKeyToHasteroidsKey k = case k of
    GLFW.Key'Space      -> HKeySpace
    GLFW.Key'Left       -> HKeyLeft
    GLFW.Key'Right      -> HKeyRight
    GLFW.Key'Up         -> HKeyUp
    GLFW.Key'Down       -> HKeyDown
    GLFW.Key'Escape     -> HKeyEsc
    _                   -> HKeyUnknown

mapGLFWKeyStateToHasteroidsKeyState :: GLFW.KeyState -> HKeyState
mapGLFWKeyStateToHasteroidsKeyState ks = case ks of
    GLFW.KeyState'Pressed   -> HKeyDownState
    GLFW.KeyState'Released  -> HKeyUpState
    GLFW.KeyState'Repeating -> HKeyDownState
