module Hasteroids.Geometry.Transform where -- Reverted

import Hasteroids.Geometry -- Reverted

translatePt :: Vec2 -> Vec2 -> Vec2
translatePt (x, y) (x', y') = (x+x', y+y')

rotatePt :: Float -> Vec2 -> Vec2
rotatePt a (x,y) = (x', y')
    where x' = x * cos a - y * sin a -- Removed unnecessary parens
          y' = x * sin a + y * cos a -- Removed unnecessary parens

applyXform :: (Vec2 -> Vec2) -> LineSegment -> LineSegment
applyXform f (LineSegment (p,p')) = LineSegment (f p, f p') -- Removed unnecessary parens
