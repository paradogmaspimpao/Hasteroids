module Hasteroids.Asteroid ( -- Reverted
    Asteroid,
    newAsteroid,
    updateAsteroid,
    Size(..)
    ) where

import Hasteroids.Geometry -- Reverted
import Hasteroids.Geometry.Body -- Reverted
import Hasteroids.Render -- Reverted
import Hasteroids.Collision -- Reverted

data Size = Small|Medium|Large deriving (Ord, Eq)
data Asteroid = Asteroid Size Body

instance LineRenderable Asteroid where
    interpolatedLines f (Asteroid sz b) = map (transform b') $ asteroidLines sz
        where b' = interpolatedBody f b

instance Collider Asteroid where
    collisionCenter (Asteroid _ b)  = bodyPos b
    collisionRadius (Asteroid sz _) = radius sz
    collisionLines = interpolatedLines 0

newAsteroid :: Vec2 -> Vec2 -> Float -> Asteroid
newAsteroid pos v r = Asteroid Large $ Body pos 0 v r pos 0

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid (Asteroid sz b) = Asteroid sz $ updateBody b

radius :: Size -> Float
radius Small  = 14
radius Medium = 28
radius Large  = 56

numVertices :: Size -> Int
numVertices Small  = 6
numVertices Medium = 8
numVertices Large  = 12

asteroidLines :: Size -> [LineSegment] -- Added type signature for clarity
asteroidLines sz = pointsToSegments $ pts sz
    where pts sz  = polarPoints (numVertices sz) (radius sz)
          polarPoints s r = map (polar r) [0.0,step..2.0*pi]
             where step = 2.0*pi/(fromIntegral s)