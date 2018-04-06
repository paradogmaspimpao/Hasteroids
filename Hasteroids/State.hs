module Hasteroids.State (GameState(..), initialGameState,) where

import Hasteroids.Player
import Hasteroids.Asteroid
import Hasteroids.Geometry
import Hasteroids.Geometry.Transform
import Hasteroids.Geometry.Body
import Hasteroids.Render (LineRenderable(..))
import Hasteroids.Tick
import Hasteroids.Keyboard

data GameState = GameState { 
	statePlayer :: Player,
	stateAsteroids :: [Asteroid] }

instance LineRenderable GameState where
    interpolatedLines f (GameState p a) = plines ++ alines
        where plines = interpolatedLines f p
              alines = concatMap (interpolatedLines f) a

initialGameState :: GameState
initialGameState = GameState {
    statePlayer = initialPlayerState,
    stateAsteroids = [
        newAsteroid (20,50) (1.5,0.7) (-0.02),
        newAsteroid (700, 10) (-1, 0.4) (-0.015)]
    }

-- Estado inicial do Jogador no centro da tela
initialPlayerState :: Player
initialPlayerState = Player $ initBody (400, 300)

instance Tickable GameState where
    tick = tickState

-- Atualiza o game-state.
tickState :: Keyboard -> GameState -> GameState
tickState kb s@(GameState pl a) = s {
    statePlayer    = tick kb pl,
    stateAsteroids = map updateAsteroid a
    }
