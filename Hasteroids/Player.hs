module Hasteroids.Player ( -- Reverted
    Player(..),
    initPlayer,
    collidePlayer) where

import Hasteroids.Controls -- Reverted
import Hasteroids.Geometry -- Reverted
import Hasteroids.Geometry.Body -- Reverted
import Hasteroids.Render (LineRenderable(..)) -- Reverted
import Hasteroids.Tick -- Reverted
import Hasteroids.Keyboard -- Reverted
import Hasteroids.Collision -- Reverted

data Player = Player {
    playerBody :: Body,
    playerAlive :: Bool
}

instance LineRenderable Player where
    interpolatedLines _ (Player _ False) = []
    interpolatedLines f (Player b _) = map (transform b') $ shipLines -- transform is from Geometry.Body
        where b' = interpolatedBody f b

instance Tickable Player where
     tick _  p@(Player _ False) = p
     tick keyboard p@(Player body _) = p { playerBody  = updatePlayerBody turn acc body }
        where turn | key keyTurnLeft  = torqueAmount
                   | key keyTurnRight = -torqueAmount
                   | otherwise     = 0
              acc | key keyThrust = thrustAmount
                   | otherwise  = 0
              key = isKeyDown keyboard -- isKeyDown from Hasteroids.Keyboard

instance Collider Player where
    collisionCenter = bodyPos . playerBody -- bodyPos from Geometry.Body
    collisionRadius = const shipSize
    collisionLines  = interpolatedLines 0

collidePlayer :: Collider a => Player -> [a] -> Player
collidePlayer p@(Player _ False) _ = p
collidePlayer p [] = p
collidePlayer p a = p { playerAlive = not $ any (collides p) a } -- collides from Hasteroids.Collision

initPlayer :: Player
initPlayer = Player (initBody (400,300)) True -- initBody from Geometry.Body

updatePlayerBody :: Float -> Float -> Body -> Body
updatePlayerBody turn acceleration = updateBody . damping 0.96 . accelerateForward acceleration . rotate turn
    -- updateBody, damping, accelerateForward, rotate are all from Geometry.Body

shipSize :: Float
shipSize = 12.0 -- Added type signature

shipLines :: [LineSegment]
shipLines = pointsToSegments points -- pointsToSegments from Hasteroids.Geometry
    where points = [polar shipSize      0, -- polar from Hasteroids.Geometry
                    polar shipSize      (0.7*pi),
                    polar (shipSize*0.2) pi,
                    polar shipSize      (1.3*pi),
                    polar shipSize      0]
