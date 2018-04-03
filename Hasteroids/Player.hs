module Hasteroids.Player (Player(..)) where

import Hasteroids.Geometry
import Hasteroids.Geometry.Transform
import Hasteroids.Render (LineRenderable(..))

data Player = Player {playerPos :: Body}

instance LineRenderable Player where
    lineSegments (Player p) = map (transform p) $ shipLines

--Constante : Tamanho da nave
shipSize = 12.0 :: Float

--Constroi a forma da nave uma unica vez. Subsequentes interações são de translado.
shipLines :: [LineSegment]
shipLines = pointsToSegments points
    where points = [polar shipSize      0,
                    polar shipSize      (0.7*pi),
                    polar (shipSize*0.2) pi,
                    polar shipSize      (1.3*pi),
                    polar shipSize      0]
