module Hasteroids.Render (LineRenderable(..)) where

import Graphics.Rendering.OpenGL
import Hasteroids.Geometry

class LineRenderable r where
    interpolatedLines :: Float -> r -> [LineSegment]
    
    renderInterpolated :: Float -> r -> IO()
    renderInterpolated f = renderLines . interpolatedLines f

-- Renderiza uma lista de segmentos de linha com OpenGL
renderLines :: [LineSegment] -> IO ()
renderLines lns = do
    currentColor $= Color4 0.9 0.9 0.9 1.0
    renderPrimitive Lines $ mapM_ lineVertices lns

--Gera os vértices de um segmento no OpenGL
lineVertices :: LineSegment -> IO ()
lineVertices (LineSegment (p,p')) = do
    ptVertex p
    ptVertex p'

--Gera um OpenGL-Vertex a partir de um ponto.
ptVertex :: Vec2 -> IO ()
ptVertex = vertex . uncurry Vertex2
