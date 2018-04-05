module Hasteroids.Player (Player(..)) where

import Hasteroids.Controls
import Hasteroids.Geometry
import Hasteroids.Geometry.Transform
import Hasteroids.Geometry.Body
import Hasteroids.Render (LineRenderable(..))
import Hasteroids.Tick
import Hasteroids.Keyboard

data Player = Player {playerBody :: Body}

instance LineRenderable Player where
    lineSegments (Player p) = map (transform p) $ shipLines

-- player needs to be tickable --
instance Tickable Player where
    tick keyboard (Player body) = Player $ updatePlayerBody turn acc body
        where turn | key turnLeft  = -0.2
                   | key turnRight = 0.2
                   | otherwise     = 0
              acc | key thrust = 1.5
                   | otherwise  = 0
                  
              key = isKeyDown keyboard

updatePlayerBody :: Float -> Float -> Body -> Body
updatePlayerBody turn acceleration = updateBody . damping 0.96 . accelerateForward acceleration . rotate turn

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
