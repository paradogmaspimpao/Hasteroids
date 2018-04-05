module Hasteroids.Keyboard (
    Keyboard,
    initKeyboard,
    handleKeyEvent,
    isKeyDown) where

import Data.Set (Set)
import qualified Data.Set as Set

import Graphics.UI.GLUT (Key(..), KeyState(..))

-- will keep a set of the keys that are currently pressed/held down
newtype Keyboard = Keyboard (Set Key)


{-
   Down: key pressed, add to the set
   Up: key released, remove from the set
-}
handleKeyEvent :: Key -> KeyState -> Keyboard -> Keyboard
handleKeyEvent key key' (Keyboard s) = case key' of
        Up -> Keyboard $ Set.delete key s
        Down -> Keyboard $ Set.insert key s

-- create a keyboard
initKeyboard :: Keyboard
initKeyboard = Keyboard Set.empty


-- check if a key is being held down
isKeyDown :: Keyboard -> Key -> Bool
isKeyDown (Keyboard s) key = Set.member key s
