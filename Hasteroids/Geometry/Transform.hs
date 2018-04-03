module Hasteroids.Geometry.Transform (
    Body (..),
    transform
) where

import Hasteroids.Geometry

data Body = Body {
    bodyPos :: Vec2,
    bodyAngle :: Float
    }
 
-- transforms a line segment based on the body position and orientation
transform :: Body -> LineSegment -> LineSegment
transform (Body pos a) = applyXform $ (translatePt pos) . (rotatePt a)

translatePt :: Vec2 -> Vec2 -> Vec2
translatePt (x, y) (x', y') = (x+x', y+y')

rotatePt :: Float -> Vec2 -> Vec2
rotatePt a (x,y) = (x', y')
    where x' = x * (cos a) - y * (sin a)
          y' = x * (sin a) + y * (cos a)


applyXform :: (Vec2 -> Vec2) -> LineSegment -> LineSegment
applyXform f (LineSegment (p,p')) = (LineSegment (f p, f p'))

