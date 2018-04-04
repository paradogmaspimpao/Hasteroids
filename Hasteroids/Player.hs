module Hasteroids.Player (Player(..)) where

import Hasteroids.Geometry
import Hasteroids.Geometry.Transform
import Hasteroids.Render (LineRenderable(..))
import Hasteroids.Tick

data Player = Player {playerBody :: Body}

instance LineRenderable Player where
    lineSegments (Player p) = map (transform p) $ shipLines

-- player needs to be tickable --
instance Tickable Player where
    tick (Player b) = Player $ rotate 0.1 b

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
