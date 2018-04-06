module Hasteroids.Asteroid (
    Asteroid,
    newAsteroid,
    updateAsteroid
    ) where

import Hasteroids.Geometry
import Hasteroids.Geometry.Body
import Hasteroids.Render

data Size = Small|Medium|Large deriving (Ord, Eq)
data Asteroid = Asteroid Size Body

instance LineRenderable Asteroid where
    interpolatedLines f (Asteroid sz b) = map (transform b') $ asteroidLines sz
        where b' = interpolatedBody f b
-- Inicializa um novo asteroide com a posicao, velocidade e rotacao dados       
newAsteroid :: Vec2 -> Vec2 -> Float -> Asteroid
newAsteroid pos v r = Asteroid Large $ Body pos 0 v r pos 0

--  Atualiza a posicao do asteroide
updateAsteroid :: Asteroid -> Asteroid
updateAsteroid (Asteroid sz b) = Asteroid sz $ updateBody b

--  o raio de um asteroide pelo seu tamanho
radius :: Size -> Float
radius Small  = 14
radius Medium = 28
radius Large  = 56

--  Pega o numero de vertices de um asteroide
numVertices :: Size -> Int
numVertices Small  = 6
numVertices Medium = 8
numVertices Large  = 12

--  Pega os segmentos de linha para o tamanho de um asteroide
asteroidLines sz = pointsToSegments $ pts sz
    where pts sz  = polarPoints (numVertices sz) (radius sz)
          polarPoints s r = map (polar r) [0.0,step..2.0*pi]
             where step = 2.0*pi/(fromIntegral s)