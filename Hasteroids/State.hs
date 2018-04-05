module Hasteroids.State (GameState(..), initialGameState,) where

import Hasteroids.Player
import Hasteroids.Geometry
import Hasteroids.Geometry.Transform
import Hasteroids.Geometry.Body
import Hasteroids.Render (LineRenderable(..))
import Hasteroids.Tick
import Hasteroids.Keyboard

data GameState = GameState { statePlayer :: Player }

instance LineRenderable GameState where
    lineSegments = stateLines

initialGameState :: GameState
initialGameState = GameState {
    statePlayer = initialPlayerState
    }

initialPlayerState :: Player
initialPlayerState = Player $ initBody (400, 300)

stateLines :: GameState -> [LineSegment]
stateLines = lineSegments . statePlayer

instance Tickable GameState where
    tick = tickState

-- Atualiza o game-state.
tickState :: Keyboard -> GameState -> GameState
tickState keyboard (GameState pl) = GameState $ tick keyboard pl
