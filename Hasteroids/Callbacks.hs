module Hasteroids.Callbacks where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Hasteroids.Render (LineRenderable(..))

-- | Render the viewport using the given renderable and swap buffers
renderViewport :: LineRenderable r => r -> IO ()
renderViewport r = do
    clear [ColorBuffer]
    render r
    swapBuffers
