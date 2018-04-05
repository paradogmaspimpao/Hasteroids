module Hasteroids.Geometry.Transform (
    Body (..),
    transform
    ) where

import Hasteroids.Geometry

--  Data type que contem a posicao e orientacao de um corpo rigido
data Body = Body {
    bodyPos   :: Vec2,
    bodyAngle :: Float
    }

-- Calcula o translado de um ponto
translate ::  Vec2 -> Vec2 -> Vec2 -- delta -> ponto original -> ponto transladado
translate (x, y) (x', y') = (x+x', y+y')

--Calcula o translado de uma linha
--translateLine :: Vec2 -> LineSegment -> LineSegment
--translateLine p (LineSegment (l, l')) = (LineSegment (t l, t l'))
  --  where t = translate p

--  Transforma um segmento de linha de acordo com a posicao e orientacao do corpo
transform :: Body -> LineSegment -> LineSegment
transform (Body pos a) = applyXform $ (translate pos) . (rotatePt a)

-- Rotaciona um ponto ao redor do origo
rotatePt :: Float -> Vec2 -> Vec2
rotatePt a (x,y) = (x', y')
    where x' = x * (cos a) - y * (sin a)
          y' = x * (sin a) + y * (cos a)

--  Aplica a funcao de transformacao a um segmento de linha
applyXform :: (Vec2 -> Vec2) -> LineSegment -> LineSegment
applyXform f (LineSegment (p,p')) = (LineSegment (f p, f p'))

