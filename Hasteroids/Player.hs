module Hasteroids.Player (
    Player(..),
    initPlayer,
    collidePlayer) where

import Hasteroids.Controls
import Hasteroids.Geometry
import Hasteroids.Geometry.Transform
import Hasteroids.Geometry.Body
import Hasteroids.Render (LineRenderable(..))
import Hasteroids.Tick
import Hasteroids.Keyboard
import Hasteroids.Collision

-- Datatype para guardar o estado atual do player
data Player = Player {
    playerBody :: Body,
    playerAlive :: Bool
}

instance LineRenderable Player where
    interpolatedLines _ (Player _ False) = []
    interpolatedLines f (Player b _) = map (transform b') $ shipLines
        where b' = interpolatedBody f b

-- Player precisa ser conforme ao "protocolo Tickable" --
instance Tickable Player where
     tick _  p@(Player _ False) = p
     tick keyboard p@(Player body _) = p { playerBody  = updatePlayerBody turn acc body }
        where turn | key turnLeft  = -0.2
                   | key turnRight = 0.2
                   | otherwise     = 0
              acc | key thrust = 1.5
                   | otherwise  = 0

              key = isKeyDown keyboard

-- Torna Player em uma instancia de collider
instance Collider Player where
    collisionCenter = bodyPos . playerBody
    collisionRadius = const shipSize
    collisionLines  = interpolatedLines 0

--  Testa colisao entre o player e uma lista de Colliders
--   Se a nave interceder com algum, ela e destruida
collidePlayer :: Collider a => Player -> [a] -> Player
collidePlayer p@(Player _ False) _ = p
collidePlayer p [] = p
collidePlayer p a = p { playerAlive = not $ any (collides p) a }

--  Estado inicial do player no centro da tela
initPlayer :: Player
initPlayer = Player (initBody (400,300)) True

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
