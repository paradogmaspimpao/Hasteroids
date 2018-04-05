module Hasteroids.Geometry.Body (
    Body (..), 
    transform,
    rotate,
    damping,
    accelerateForward,
    updateBody, 
    initBody
    ) where

import Hasteroids.Geometry
import Hasteroids.Geometry.Transform

data Body = Body {
    bodyPos :: Vec2,
    bodyAngle :: Float,

    bodyVelocity :: Vec2,
    bodyRotation :: Float
    }

-- initialize body
initBody :: Vec2 -> Body
initBody pos = Body pos 0 (0, 0) 0

-- update position and orientation of a body, according to its velocity and rotation
updateBody :: Body -> Body
updateBody body = body {bodyPos = pos, bodyAngle = angle}
    where pos = bodyPos body /+/ bodyVelocity body
          angle = bodyAngle body + bodyRotation body

-- accelerate body with a vector
accelerate :: Vec2 -> Body -> Body
accelerate (ax, ay) body = body { bodyVelocity = newVelocity }
    where newVelocity = (ax+vx, ay+vy)
          (vx, vy) = bodyVelocity body

accelerateForward :: Float -> Body -> Body
accelerateForward mag body = accelerate (polar mag $ bodyAngle body) body

-- damping effect
damping :: Float -> Body -> Body
damping coefficient body = body { bodyVelocity = coefficient */ bodyVelocity body}

rotate :: Float -> Body -> Body
rotate n b = b { bodyRotation = n }

transform :: Body -> LineSegment -> LineSegment
transform (Body pos a _ _) = applyXform $ (translatePt pos) . (rotatePt a)

