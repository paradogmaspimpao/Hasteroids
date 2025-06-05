module Hasteroids.Render (LineRenderable(..), renderLinesModern) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array (withArray)
import Foreign.Storable (sizeOf)
-- import Data.Foldable (concatMap) -- concatMap is in Prelude
import Control.Monad (when)

import Hasteroids.Geometry
import Hasteroids.Geometry.Transform
import Hasteroids.Shader (ShaderProgram)

class LineRenderable r where
    interpolatedLines :: Float -> r -> [LineSegment]

renderLinesModern :: ShaderProgram
                  -> GL.VertexArrayObject
                  -> GL.BufferObject
                  -> GL.GLmatrix GL.GLfloat
                  -> [LineSegment]
                  -> IO ()
renderLinesModern program vao vbo _projectionMatrix segments = do
    GL.currentProgram $= Just program
    GL.bindVertexArrayObject $= Just vao
    GL.bindBuffer GL.ArrayBuffer $= Just vbo

    let wrappedSegments = wrapLines segments
        vertexData = concatMap segmentToFloats wrappedSegments -- concatMap is Prelude
        numVertices = fromIntegral (length vertexData `div` 2)

    if null vertexData
    then return ()
    else withArray vertexData $ \ptr ->
        GL.bufferData GL.ArrayBuffer $= (fromIntegral (length vertexData * sizeOf (0::GL.GLfloat)), ptr, GL.StreamDraw)

    lineColorLocation <- GL.get (GL.uniformLocation program "lineColor")
    GL.uniform lineColorLocation $= GL.Color4 0.9 0.9 0.9 (1.0 :: GL.GLfloat)

    when (numVertices > 0) $ GL.drawArrays GL.Lines 0 numVertices

    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.bindVertexArrayObject $= Nothing
    GL.currentProgram $= Nothing

segmentToFloats :: LineSegment -> [GL.GLfloat]
segmentToFloats (LineSegment ((x1,y1),(x2,y2))) = [realToFrac x1, realToFrac y1, realToFrac x2, realToFrac y2]

wrapLines :: [LineSegment] -> [LineSegment]
wrapLines = foldr go []
    where go l@(LineSegment (p,p')) acc
                | both      = l':l'':acc
                | first     = l:l':acc
                | second    = l:l'':acc
                | otherwise = l:acc
            where
              both   = first && second && w/= w'
              first  = (w /= (0,0))
              second = (w' /= (0,0))

              w   = Hasteroids.Geometry.wrapper p
              w'  = Hasteroids.Geometry.wrapper p'
              l'  = Hasteroids.Geometry.Transform.applyXform (Hasteroids.Geometry.Transform.translatePt w) l
              l'' = Hasteroids.Geometry.Transform.applyXform (Hasteroids.Geometry.Transform.translatePt w') l
