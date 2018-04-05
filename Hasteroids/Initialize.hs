module Hasteroids.Initialize where

import Data.IORef

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Hasteroids.State (initialGameState)
import Hasteroids.Callbacks
import Hasteroids.Keyboard

initializeWindow :: IO Window
initializeWindow = do
    _ <- getArgsAndInitialize
    initialWindowSize  $= Size 800 600
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Hasteroids"


initializeOpenGL = do

    depthMask $= Disabled


    lineSmooth  $= Enabled
    blend       $= Enabled
    blendFunc   $= (SrcAlpha,OneMinusSrcAlpha)
    lineWidth   $= 2.0


    viewport   $= (Position 0 0, Size 800 600)


    matrixMode $= Projection
    loadIdentity
    ortho 0 800 600 0 (-1) 1
    matrixMode $= Modelview 0
    loadIdentity


    clearColor $= Color4 0.0 0.0 0.1 1.0

initializeCallbacks :: IO ()
initializeCallbacks = do
    keyboard <- newIORef initKeyboard
    keyboardMouseCallback $= Just (handleKeyboard keyboard)
    displayCallback $= renderViewport initialGameState
    addTimerCallback 0 $ logicTick keyboard initialGameState
