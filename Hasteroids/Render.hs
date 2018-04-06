module Hasteroids.Render (LineRenderable(..)) where

import Graphics.Rendering.OpenGL
import Hasteroids.Geometry
import Hasteroids.Geometry.Transform

class LineRenderable r where
    interpolatedLines :: Float -> r -> [LineSegment]

    renderInterpolated :: Float -> r -> IO()
    renderInterpolated f = renderLines . interpolatedLines f

-- Renderiza uma lista de segmentos de linha com OpenGL
renderLines :: [LineSegment] -> IO ()
renderLines lns = do
    currentColor $= Color4 0.9 0.9 0.9 1.0
    renderPrimitive Lines $ mapM_ lineVertices $ wrapLines lns

-- | Generate extra lines for segments that go out of the screen
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

              w   = wrapper p
              w'  = wrapper p'
              l'  = applyXform (translatePt w) l
              l'' = applyXform (translatePt w') l

--Gera os vértices de um segmento no OpenGL
lineVertices :: LineSegment -> IO ()
lineVertices (LineSegment (p,p')) = do
    ptVertex p
    ptVertex p'

--Gera um OpenGL-Vertex a partir de um ponto.
ptVertex :: Vec2 -> IO ()
ptVertex = vertex . uncurry Vertex2
