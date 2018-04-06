module Hasteroids.Callbacks (
      initCallbackRefs,
      renderViewport,
      handleKeyboard)where

import Data.IORef
import Data.Time.Clock.POSIX

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Hasteroids.Render (LineRenderable(..))
import Hasteroids.Tick
import Hasteroids.Keyboard
import Hasteroids.State (GameState, initialGameState)

type KeyboardRef = IORef Keyboard
type TimeRef     = IORef POSIXTime
type StateRef    = IORef GameState


data CallbackRefs = CallbackRefs TimeRef TimeRef KeyboardRef StateRef


--  Inicializa um novo grupo de referencias de callback
initCallbackRefs :: IO CallbackRefs
initCallbackRefs = do
    accum <- newIORef $ 0
    prev  <- getPOSIXTime >>= newIORef
    keyb  <- newIORef initKeyboard
    st    <- newIORef initialGameState
    return $ CallbackRefs accum prev keyb st

--  Roda a logica do jogo,renderiza a view e troca os buffers de display
renderViewport :: CallbackRefs -> IO ()
renderViewport refs@(CallbackRefs ar tr kb rr) = do
    current <- getPOSIXTime
    prev <- readIORef tr
    accum <- readIORef ar
    keys <- readIORef kb
    
    let frameTime = min 0.1 $ current - prev
        newAccum  = accum + frameTime

    let consumeAccum acc = if acc >= 0.033
            then do
               modifyIORef rr $ tick keys
               consumeAccum $ acc - 0.033
            else return acc
    
    newAccum' <- consumeAccum newAccum
    
    writeIORef tr current
    writeIORef ar newAccum'

    let interpolation = realToFrac $ newAccum' / 0.0333
    
    r <- readIORef rr
    
    clear [ColorBuffer]
    renderInterpolated interpolation r
    swapBuffers
    postRedisplay Nothing

-- Atualiza o estado do teclado de acordo com um evento
-- KeyboardMouseCallback é um alias para:
-- Key -> KeyState -> Modifiers -> Position -> IO())
handleKeyboard :: CallbackRefs -> KeyboardMouseCallback
handleKeyboard (CallbackRefs _ _ keyboard _) key key' _ _ = modifyIORef keyboard (handleKeyEvent key key')
