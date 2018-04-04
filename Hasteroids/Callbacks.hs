module Hasteroids.Callbacks where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Hasteroids.Render (LineRenderable(..))
import Hasteroids.Tick

-- | Render the viewport using the given renderable and swap buffers
renderViewport :: LineRenderable r => r -> IO ()
renderViewport r = do
    clear [ColorBuffer]
    render r
    swapBuffers

logicTick :: (LineRenderable t, Tickable t) => t -> IO ()
logicTick t = do
    let newTickable = tick t
    displayCallback $= renderViewport newTickable
    addTimerCallback 34 $ logicTick newTickable
    postRedisplay Nothing
