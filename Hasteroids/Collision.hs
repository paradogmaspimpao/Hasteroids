module Hasteroids.Collision (Collider(..)) where

-- import Control.Applicative -- Removed unused import

import Hasteroids.Geometry

class Collider c where
    collisionLines :: c -> [LineSegment]
    collisionCenter :: c -> Vec2
    collisionRadius :: c -> Float
    
    collides :: (Collider d) => c -> d -> Bool
    collides c c' = canCollide && doesCollide
        where canCollide  = distSqr < radius*radius
              -- The (<$>) operator was from Control.Applicative,
              -- but `fmap` (which is Prelude) or list comprehension can be used.
              -- `or $ map (uncurry lineCollision) $ liftA2 (,) cl cl'` would also work with Applicative.
              -- For simplicity, using list comprehension or direct map.
              -- doesCollide = or $ [lineCollision s1 s2 | s1 <- cl, s2 <- cl'] -- Alternative
              doesCollide = any id $ map (\s1 -> any (lineCollision s1) cl') cl -- More direct without list comp.
              
              distSqr = ptDistanceSqr (collisionCenter c) (collisionCenter c')
              radius  = collisionRadius c + collisionRadius c'
              cl  = collisionLines c
              cl' = collisionLines c'

lineCollision :: LineSegment -> LineSegment -> Bool
lineCollision (LineSegment ((x1,y1),(x2,y2))) (LineSegment ((x3,y3),(x4,y4))) =
    if d == 0
        then False
        else ua >= 0 && ua <= 1 && ub >= 0 && ub <= 1
    where ua = na/d
          ub = nb/d
          d  = (y4-y3)*(x2-x1) - (x4-x3)*(y2-y1)
          na = (x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)
          nb = (x2-x1)*(y1-y3) - (y2-y1)*(x1-x3)