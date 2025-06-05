module Hasteroids.Callbacks (
      initCallbackRefs,
      runGameLogicAndRender,
      updateKeyboardState,
      CallbackRefs(..)
      ) where

import Data.IORef
import Data.Time.Clock.POSIX

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Hasteroids.Render (LineRenderable(..), renderLinesModern)
import Hasteroids.Tick
-- Removed HKeyState(..) from import as it was unused
import Hasteroids.Keyboard (Keyboard, HKey(..), handleKeyEvent, mapGLFWKeyToHasteroidsKey, mapGLFWKeyStateToHasteroidsKeyState, initKeyboard)
import Hasteroids.State (GameState, initialGameState)
import Hasteroids.Initialize (RenderContext(..))
-- Removed import Hasteroids.Geometry (LineSegment) as LineSegment is likely in scope via Hasteroids.Render or not needed directly

type KeyboardRef      = IORef Keyboard
type TimeRef          = IORef POSIXTime
type StateRef         = IORef GameState
type RenderContextRef = IORef RenderContext

data CallbackRefs = CallbackRefs
    { cbAccumulatorRef :: TimeRef
    , cbLastTimeRef    :: TimeRef
    , cbKeyboardRef    :: KeyboardRef
    , cbGameStateRef   :: StateRef
    , cbRenderCtxRef   :: RenderContextRef
    }

initCallbackRefs :: RenderContext -> IO (IORef CallbackRefs)
initCallbackRefs renderCtx = do
    accum <- newIORef 0
    prev  <- getPOSIXTime >>= newIORef
    keyb  <- newIORef initKeyboard
    st    <- newIORef initialGameState
    rc    <- newIORef renderCtx
    newIORef $ CallbackRefs accum prev keyb st rc

runGameLogicAndRender :: IORef CallbackRefs -> IO ()
runGameLogicAndRender refsIORef = do
    refs <- readIORef refsIORef
    let ar = cbAccumulatorRef refs
        tr = cbLastTimeRef refs
        kb = cbKeyboardRef refs
        rr = cbGameStateRef refs
        rcRef = cbRenderCtxRef refs

    renderCtx <- readIORef rcRef
    current <- getPOSIXTime
    prev <- readIORef tr
    accum <- readIORef ar
    
    let frameTime = min 0.1 $ current - prev
        newAccum  = accum + frameTime

    currentKeys_ <- readIORef kb
    let consumeAccum nAcc =
            if nAcc >= 0.033 then do
                modifyIORef rr $ tick currentKeys_
                consumeAccum (nAcc - 0.033)
            else return nAcc
    
    newAccum' <- consumeAccum newAccum
    
    writeIORef tr current
    writeIORef ar newAccum'

    let interpolation = realToFrac $ newAccum' / 0.0333
    currentGameState <- readIORef rr
    let segments = interpolatedLines interpolation currentGameState

    GL.clear [GL.ColorBuffer]
    
    currentProjMatrix <- readIORef (rcProjectionMatrixRef renderCtx)
    renderLinesModern (rcShaderProgram renderCtx)
                      (rcVAO renderCtx)
                      (rcVBO renderCtx)
                      currentProjMatrix
                      segments

updateKeyboardState :: IORef Keyboard -> GLFW.Key -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
updateKeyboardState keyboardRef glfwKey glfwKeyState _mods = do -- Changed mods to _mods
    let hasteroidsKey = mapGLFWKeyToHasteroidsKey glfwKey
    case hasteroidsKey of
        HKeyUnknown -> return ()
        _ -> do
            let hasteroidsKeyState = mapGLFWKeyStateToHasteroidsKeyState glfwKeyState
            modifyIORef keyboardRef (handleKeyEvent hasteroidsKey hasteroidsKeyState)
