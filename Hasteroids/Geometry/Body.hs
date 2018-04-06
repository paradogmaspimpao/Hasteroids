module Hasteroids.Geometry.Body (
    Body (..),
    transform,
    rotate,
    damping,
    accelerateForward,
    updateBody,
    initBody,
    interpolatedBody,
    ) where

import Hasteroids.Geometry
import Hasteroids.Geometry.Transform

data Body = Body {
    bodyPos :: Vec2,
    bodyAngle :: Float,

    bodyVelocity :: Vec2,
    bodyRotation :: Float,

    prevPos   :: Vec2,
    prevAngle :: Float
    }

-- Inicializa o corpo
initBody :: Vec2 -> Body
initBody pos = Body pos 0 (0, 0) 0 pos 0

-- Atualiza posição e orientação do corpo de acordo com sua velocidade e rotação
updateBody :: Body -> Body
updateBody b = b {
     bodyPos = pos' /+/ wrap,  bodyAngle = a',
     prevPos = pos  /+/ wrap,  prevAngle = a }

     where a    = bodyAngle b
           pos  = bodyPos b
           pos' = pos /+/ bodyVelocity b
           a'   = a + bodyRotation b
           wrap = wrapper pos'

--  Generate body data is is between current and previous state.
interpolatedBody :: Float -- ^ interpolation point
                 -> Body  -- ^ body
                 -> Body  -- ^ interpolated body
interpolatedBody i body = body { bodyPos = pos', bodyAngle = angle' }
    where pos' = (bodyPos body) /* i /+/ (prevPos body) /* i'
          angle'   = (bodyAngle body) * i + (prevAngle body) * i'
          i'   = 1.0 - i

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
transform (Body pos a _ _ _ _) = applyXform $ (translatePt pos) . (rotatePt a)
