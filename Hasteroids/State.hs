module Hasteroids.State (GameState(..), initialGameState) where -- Reverted

import Hasteroids.Player -- Reverted
import Hasteroids.Asteroid -- Reverted
import Hasteroids.Geometry -- Reverted
import Hasteroids.Render (LineRenderable(..)) -- Reverted
import Hasteroids.Tick -- Reverted
import Hasteroids.Keyboard -- Reverted

data GameState = GameState {
	statePlayer :: Player,
	stateAsteroids :: [Asteroid] }

instance LineRenderable GameState where
    interpolatedLines f (GameState p a) = plines ++ alines
        where plines = interpolatedLines f p -- from Hasteroids.Player instance
              alines = concatMap (interpolatedLines f) a -- from Hasteroids.Asteroid instance

initialGameState :: GameState
initialGameState = GameState {
    statePlayer = initPlayer, -- from Hasteroids.Player
    stateAsteroids = [
        newAsteroid (20,50) (1.5,0.7) (-0.02), -- from Hasteroids.Asteroid
        newAsteroid (700, 10) (-1, 0.4) (-0.015),
		newAsteroid (2,500) (1.2,0.3) (-0.04),
		newAsteroid (78,300) (-1,0.7) (-0.05),
		newAsteroid (200,400) (-1,-0.7) (-0.025)
    ]
}

instance Tickable GameState where
    tick = tickState -- tick from Hasteroids.Tick, tickState is local

tickState :: Keyboard -> GameState -> GameState
tickState kb s@(GameState pl a) = s {
    statePlayer    = collidePlayer p' a', -- collidePlayer from Hasteroids.Player
    stateAsteroids = a'
    }
    where  p' = tick kb pl -- tick from Hasteroids.Tick (Player instance)
           a' = map updateAsteroid a -- updateAsteroid from Hasteroids.Asteroid
