module Hasteroids.Geometry where -- Reverted

type VecVal = Float
type Vec2 = (VecVal, VecVal)
newtype LineSegment = LineSegment (Vec2, Vec2)

polar :: VecVal -> VecVal -> Vec2
polar m a = (m * sin a, m * (-cos a))

pointsToSegments :: [Vec2] -> [LineSegment]
pointsToSegments [] = []
pointsToSegments [_] = []
pointsToSegments (p:p':[]) = [LineSegment (p, p')]
pointsToSegments (p:t@(p':_)) = LineSegment (p, p') : pointsToSegments t -- Corrected pattern match

wrapper :: Vec2 -> Vec2
wrapper (x,y) = (x',y')
    where x' | x < 0 = 800
             | x >= 800 = -800
             | otherwise = 0
          y' | y < 0 = 600
             | y >= 600 = -600
             | otherwise = 0

ptDistanceSqr :: Vec2 -> Vec2 -> VecVal
ptDistanceSqr (x,y) (x',y') = dx*dx + dy*dy
    where dx = x-x'
          dy = y-y'

(^+^) :: Vec2 -> Vec2 -> Vec2
(x, y) ^+^ (x1, y1) = (x+x1, y+y1)

infixl 6 ^+^

(*^) :: VecVal -> Vec2 -> Vec2
n *^ (x, y) = (n*x, n*y)

(^*) :: Vec2 -> VecVal -> Vec2
(x, y) ^* n = (n*x, n*y)

infixl 7 *^
infixl 7 ^*
