module Hasteroids.Controls ( -- Reverted
    thrustAmount,
    torqueAmount,
    bulletVelocity,
    keyTurnLeft,
    keyTurnRight,
    keyThrust,
    keyShoot
) where

import Hasteroids.Keyboard (HKey(..)) -- Reverted

thrustAmount :: Float
thrustAmount = 2.5

torqueAmount :: Float
torqueAmount = 0.05

bulletVelocity :: Float
bulletVelocity = 250.0

keyTurnLeft :: HKey
keyTurnLeft = HKeyLeft

keyTurnRight :: HKey
keyTurnRight = HKeyRight

keyThrust :: HKey
keyThrust = HKeyUp

keyShoot :: HKey
keyShoot = HKeySpace
