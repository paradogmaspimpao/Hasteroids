module Hasteroids.Shader (loadShaders, ShaderProgram) where -- Reverted

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString as B
import Control.Monad (unless)

type ShaderProgram = GL.Program

loadShaders :: FilePath -> FilePath -> IO ShaderProgram
loadShaders vertPath fragPath = do
    vertShader <- GL.createShader GL.VertexShader
    fragShader <- GL.createShader GL.FragmentShader

    vertSource <- B.readFile vertPath
    fragSource <- B.readFile fragPath

    GL.shaderSourceBS vertShader $= vertSource
    GL.compileShader vertShader
    vertCompiled <- GL.get (GL.compileStatus vertShader)
    unless vertCompiled $ do
        infoLog <- GL.get (GL.shaderInfoLog vertShader)
        putStrLn $ "Vertex shader compilation failed:\n" ++ infoLog
        error "Vertex shader compilation error"

    GL.shaderSourceBS fragShader $= fragSource
    GL.compileShader fragShader
    fragCompiled <- GL.get (GL.compileStatus fragShader)
    unless fragCompiled $ do
        infoLog <- GL.get (GL.shaderInfoLog fragShader)
        putStrLn $ "Fragment shader compilation failed:\n" ++ infoLog
        error "Fragment shader compilation error"

    program <- GL.createProgram
    GL.attachShader program vertShader
    GL.attachShader program fragShader
    GL.linkProgram program
    linked <- GL.get (GL.linkStatus program)
    unless linked $ do
        infoLog <- GL.get (GL.programInfoLog program)
        putStrLn $ "Shader program linking failed:\n" ++ infoLog
        error "Shader program linking error"

    GL.detachShader program vertShader
    GL.detachShader program fragShader
    GL.deleteObjectName vertShader
    GL.deleteObjectName fragShader

    return program
