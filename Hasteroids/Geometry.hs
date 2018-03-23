module Hasteroids.Geometry where

    -- Alias para valor fundamental do vetor
    type VecVal = Float
    --Alias para vetor 2D
    type Vec2 = (VecVal, VecVal)
    --Definição própria de segmento de linha, baseado no valor absoluto de 2 vetores.
    newtype LineSegment = LineSegment (Vec2, Vec2)
