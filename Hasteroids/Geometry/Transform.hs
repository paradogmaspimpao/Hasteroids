module Hasteroids.Geometry.Transform where

import Hasteroids.Geometry

translatePt :: Vec2 -> Vec2 -> Vec2
translatePt (x, y) (x', y') = (x+x', y+y')

rotatePt :: Float -> Vec2 -> Vec2
rotatePt a (x,y) = (x', y')
    where x' = x * (cos a) - y * (sin a)
          y' = x * (sin a) + y * (cos a)

--  Aplica a funcao de transformacao a um segmento de linha
applyXform :: (Vec2 -> Vec2) -> LineSegment -> LineSegment
applyXform f (LineSegment (p,p')) = (LineSegment (f p, f p'))
