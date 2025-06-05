module Hasteroids.Initialize where

import Data.IORef
-- import Control.Monad (unless) -- Removed unused import
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Foreign.Ptr (nullPtr)

-- Removed HKeyState(..) from import as it was unused
import Hasteroids.Keyboard (Keyboard, HKey(..), handleKeyEvent, mapGLFWKeyToHasteroidsKey, mapGLFWKeyStateToHasteroidsKeyState)
import Hasteroids.Shader (loadShaders, ShaderProgram)

data RenderContext = RenderContext
    { rcShaderProgram :: ShaderProgram
    , rcVAO :: GL.VertexArrayObject
    , rcVBO :: GL.BufferObject
    , rcProjectionMatrixRef :: IORef (GL.GLmatrix GL.GLfloat)
    }

initializeWindow :: IO GLFW.Window
initializeWindow = do
    GLFW.defaultWindowHints
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True

    maybeWindow <- GLFW.createWindow 800 600 "Hasteroids - Blackholes" Nothing Nothing
    case maybeWindow of
        Nothing -> do
            putStrLn "Failed to create GLFW window"
            GLFW.terminate
            error "GLFW window creation failed"
        Just win -> do
            GLFW.makeContextCurrent (Just win)
            return win

initializeOpenGL :: IO RenderContext
initializeOpenGL = do
    GL.depthMask $= GL.Disabled
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.clearColor $= GL.Color4 0.0 0.0 0.1 1.0

    shaderProg <- loadShaders "shaders/simple.vert" "shaders/simple.frag"
    GL.currentProgram $= Just shaderProg

    vao <- GL.genObjectName :: IO GL.VertexArrayObject
    GL.bindVertexArrayObject $= Just vao

    vbo <- GL.genObjectName :: IO GL.BufferObject
    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.vertexAttribPointer (GL.AttribLocation 0) $=
        (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.bindVertexArrayObject $= Nothing

    initialProjMatrix <- orthoMatrix 0 800 600 0 (-1) 1
    projMatrixLocation <- GL.get (GL.uniformLocation shaderProg "projection")
    GL.uniform projMatrixLocation $= initialProjMatrix

    GL.viewport $= (GL.Position 0 0, GL.Size 800 600)
    projMatrixRef <- newIORef initialProjMatrix

    return $ RenderContext shaderProg vao vbo projMatrixRef

orthoMatrix :: Float -> Float -> Float -> Float -> Float -> Float -> IO (GL.GLmatrix Float)
orthoMatrix l r b t n f =
    let rl = r - l; tb = t - b; fn = f - n
        tx = -(r + l) / rl; ty = -(t + b) / tb; tz = -(f + n) / fn
    in GL.newMatrix GL.ColumnMajor [2/rl,0,0,0,  0,2/tb,0,0,  0,0,-2/fn,0,  tx,ty,tz,1]

initializeGlfwCallbacks :: GLFW.Window
                        -> IORef Keyboard
                        -> ShaderProgram
                        -> IORef (GL.GLmatrix GL.GLfloat)
                        -> IO ()
initializeGlfwCallbacks window keyboardRef shaderProg projMatrixRef = do
    GLFW.setKeyCallback window (Just (glfwKeyCallback keyboardRef))
    GLFW.setWindowSizeCallback window (Just (resizeViewport shaderProg projMatrixRef))

glfwKeyCallback :: IORef Keyboard -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
glfwKeyCallback keyboardRef _win key _scancode keyState _mods = do
    let mappedKey = mapGLFWKeyToHasteroidsKey key
    case mappedKey of
        HKeyUnknown -> return ()
        _ -> do
            let mappedKeyState = mapGLFWKeyStateToHasteroidsKeyState keyState
            modifyIORef keyboardRef (Hasteroids.Keyboard.handleKeyEvent mappedKey mappedKeyState)

resizeViewport :: ShaderProgram -> IORef (GL.GLmatrix GL.GLfloat) -> GLFW.Window -> Int -> Int -> IO ()
resizeViewport program projMatrixRef _win newWidth newHeight = do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral newWidth) (fromIntegral newHeight))
    newProjMatrix <- orthoMatrix 0 (fromIntegral newWidth) (fromIntegral newHeight) 0 (-1) 1
    writeIORef projMatrixRef newProjMatrix
    GL.currentProgram $= Just program
    projMatrixLocation <- GL.get (GL.uniformLocation program "projection")
    GL.uniform projMatrixLocation $= newProjMatrix
