import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (unless)
import Data.IORef (readIORef)

import Hasteroids.Initialize (initializeWindow, initializeOpenGL, initializeGlfwCallbacks, RenderContext(..))
import Hasteroids.Callbacks (runGameLogicAndRender, initCallbackRefs, CallbackRefs(..))

main :: IO () -- Added type signature
main = do
    True <- GLFW.init
    window <- initializeWindow
    renderContext <- initializeOpenGL

    callbackRefsIORef <- initCallbackRefs renderContext

    refs <- readIORef callbackRefsIORef
    let keyboardRef = cbKeyboardRef refs
        shaderProg = rcShaderProgram renderContext
        projMatrixRef = rcProjectionMatrixRef renderContext

    initializeGlfwCallbacks window keyboardRef shaderProg projMatrixRef

    let loop = do
            GLFW.pollEvents
            runGameLogicAndRender callbackRefsIORef
            GLFW.swapBuffers window
            shouldClose <- GLFW.windowShouldClose window
            unless shouldClose loop
    loop

    GLFW.terminate
