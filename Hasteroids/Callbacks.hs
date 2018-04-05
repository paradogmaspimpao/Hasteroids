module Hasteroids.Callbacks where

import Data.IORef

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Hasteroids.Render (LineRenderable(..))
import Hasteroids.Tick
import Hasteroids.Keyboard

type KeyboardRef = IORef Keyboard

-- | Renderiza a viewport e troca os buffers
renderViewport :: LineRenderable r => r -> IO ()
renderViewport r = do
    clear [ColorBuffer]
    render r
    swapBuffers

logicTick :: (LineRenderable t, Tickable t) => KeyboardRef -> t -> IO ()
logicTick keyboard t = do
    keys <- readIORef keyboard
    let newTickable = tick keys t
    displayCallback $= renderViewport newTickable
    addTimerCallback 33 $ logicTick keyboard newTickable
    postRedisplay Nothing


-- Atualiza o estado do teclado de acordo com um evento
-- KeyboardMouseCallback é um alias para:
-- Key -> KeyState -> Modifiers -> Position -> IO())
handleKeyboard :: KeyboardRef -> KeyboardMouseCallback
handleKeyboard keyboard key key' _ _ = modifyIORef keyboard (handleKeyEvent key key')
