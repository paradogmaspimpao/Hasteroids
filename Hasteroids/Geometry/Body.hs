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

-- Inicializa o corpo
initBody :: Vec2 -> Body
initBody pos = Body pos 0 (0, 0) 0

-- Atualiza posição e orientação do corpo de acordo com sua velocidade e rotação
updateBody :: Body -> Body
updateBody body = body {bodyPos = pos, bodyAngle = angle}
    where pos = bodyPos body /+/ bodyVelocity body
          angle = bodyAngle body + bodyRotation body

-- Acelera o corpo a partir de um vetor
accelerate :: Vec2 -> Body -> Body
accelerate (ax, ay) body = body { bodyVelocity = newVelocity }
    where newVelocity = (ax+vx, ay+vy)
          (vx, vy) = bodyVelocity body

accelerateForward :: Float -> Body -> Body
accelerateForward mag body = accelerate (polar mag $ bodyAngle body) body

-- Desacelera o corpo com um efeito de resistência ao movimento.
damping :: Float -> Body -> Body
damping coefficient body = body { bodyVelocity = coefficient */ bodyVelocity body}

rotate :: Float -> Body -> Body
rotate n b = b { bodyRotation = n }

transform :: Body -> LineSegment -> LineSegment
transform (Body pos a _ _) = applyXform $ (translatePt pos) . (rotatePt a)
