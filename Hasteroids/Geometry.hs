module Hasteroids.Geometry where

-- Alias para valor fundamental do vetor
type VecVal = Float
--Alias para vetor 2D
type Vec2 = (VecVal, VecVal)
--Definição própria de segmento de linha, baseado no valor absoluto de 2 vetores.
newtype LineSegment = LineSegment (Vec2, Vec2)

-- Converte de polar para cartesiano com referencial 0 = cima, pi/2 = direita
polar :: VecVal -> VecVal -> Vec2 -- Coord Radial -> Coord Angular -> Ponto Cartesiano
polar m a = (m * sin a, m * (-cos a))

-- Transforma uma lista de pontos em uma lista de segmentos conectados.
pointsToSegments :: [Vec2] -> [LineSegment]
pointsToSegments (p:p':[]) = [LineSegment (p, p')]
pointsToSegments (p:t@(p':ps)) = (LineSegment (p, p')) : pointsToSegments t
{-
-- Calcula o translado de um ponto
translate ::  Vec2 -> Vec2 -> Vec2 -- delta -> ponto original -> ponto transladado
translate (x, y) (x', y') = (x+x', y+y')

--Calcula o translado de uma linha
translateLine :: Vec2 -> LineSegment -> LineSegment
translateLine p (LineSegment (l, l')) = (LineSegment (t l, t l'))
    where t = translate p
-}
