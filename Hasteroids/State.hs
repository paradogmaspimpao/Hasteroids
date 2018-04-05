module Hasteroids.State (GameState(..), initialGameState,) where

import Hasteroids.Player
import Hasteroids.Geometry
import Hasteroids.Geometry.Transform
import Hasteroids.Render (LineRenderable(..))

data GameState = GameState { statePlayer :: Player }

instance LineRenderable GameState where
    lineSegments = stateLines

initialGameState :: GameState
initialGameState = GameState {
    statePlayer = initialPlayerState
    }

initialPlayerState :: Player
initialPlayerState = Player $ Body {bodyPos=(400, 300), bodyAngle=pi/4.0}

stateLines :: GameState -> [LineSegment]
stateLines = lineSegments . statePlayer
