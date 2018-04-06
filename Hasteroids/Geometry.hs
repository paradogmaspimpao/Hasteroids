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

-- | Give a delta vector that needs to be added to point to wrap it around the
--   screen edge.
wrapper :: Vec2 -> Vec2
wrapper (x,y) = (x',y')
    where x' | x < 0 = 800
             | x >= 800 = -800
             | otherwise = 0
          y' | y < 0 = 600
             | y >= 600 = -600
             | otherwise = 0

-- Adição entre dois vetores
(x, y) /+/ (x1, y1) = (x+x1, y+y1)

infixl 6 /+/

-- multiplicação de um vetor por um escalar
-- / Indica o lado do vetor
n */ (x, y) = (n*x, n*y)
(x, y) /* n = (n*x, n*y)

infixl 7 /*
infixl 7 */
