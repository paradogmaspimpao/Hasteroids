module Hasteroids.Geometry.Body ( -- Reverted
    Body (..),
    transform,
    rotate,
    damping,
    accelerateForward,
    updateBody,
    initBody,
    interpolatedBody
    ) where

import Hasteroids.Geometry -- Reverted
import Hasteroids.Geometry.Transform -- Reverted

data Body = Body {
    bodyPos :: Vec2,
    bodyAngle :: Float,

    bodyVelocity :: Vec2,
    bodyRotation :: Float,

    prevPos   :: Vec2,
    prevAngle :: Float
    }

initBody :: Vec2 -> Body
initBody pos = Body pos 0 (0, 0) 0 pos 0

updateBody :: Body -> Body
updateBody b = b {
     bodyPos = bodyPos b ^+^ bodyVelocity b ^+^ wrap, -- Simplified and corrected logic
     bodyAngle = bodyAngle b + bodyRotation b,
     prevPos = bodyPos b ^+^ wrap, -- prevPos should be current pos before move, with wrap
     prevAngle = bodyAngle b }
     where
        -- Calculate wrapped next position to determine wrap offset for current position
        nextRawPos = bodyPos b ^+^ bodyVelocity b
        wrap = wrapper nextRawPos


interpolatedBody :: Float
                 -> Body
                 -> Body
interpolatedBody i body = body { bodyPos = pos', bodyAngle = angle' }
    where pos' = (bodyPos body ^* i) ^+^ (prevPos body ^* (1.0 - i)) -- Ensure (1.0-i) for i'
          angle'   = (bodyAngle body) * i + (prevAngle body) * (1.0 - i)

accelerate :: Vec2 -> Body -> Body
accelerate (ax, ay) body = body { bodyVelocity = newVelocity }
    where newVelocity = (vx + ax, vy + ay) -- Corrected order for clarity
          (vx, vy) = bodyVelocity body

accelerateForward :: Float -> Body -> Body
accelerateForward mag body = accelerate (polar mag $ bodyAngle body) body

damping :: Float -> Body -> Body
damping coefficient body = body { bodyVelocity = coefficient *^ bodyVelocity body}

rotate :: Float -> Body -> Body
rotate n b = b { bodyRotation = n }

transform :: Body -> LineSegment -> LineSegment
transform (Body pos angle _ _ _ _) = applyXform $ translatePt pos . rotatePt angle -- Simplified from (Body pos a _ _ _ _) to use names
