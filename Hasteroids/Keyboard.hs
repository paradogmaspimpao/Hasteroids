module Hasteroids.Keyboard (
    Keyboard,
    initKeyboard,
    handleKeyEvent,
    isKeyDown) where

import Data.Set (Set)
import qualified Data.Set as Set

import Graphics.UI.GLUT (Key(..), KeyState(..))

-- Mantém um set das teclas que estão apertadas atualmente.
newtype Keyboard = Keyboard (Set Key)


{-
   Down: tecla pressionada, adiciona ao set.
   Up: Tecla solta, remover do set.
-}
handleKeyEvent :: Key -> KeyState -> Keyboard -> Keyboard
handleKeyEvent key key' (Keyboard s) = case key' of
        Up -> Keyboard $ Set.delete key s
        Down -> Keyboard $ Set.insert key s

-- Cria uma instância de teclado.
initKeyboard :: Keyboard
initKeyboard = Keyboard Set.empty


-- Checa se uma tecla esta sendo pressionada.
isKeyDown :: Keyboard -> Key -> Bool
isKeyDown (Keyboard s) key = Set.member key s
