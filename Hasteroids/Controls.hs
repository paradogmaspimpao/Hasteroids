module Hasteroids.Controls (
    turnRight,
    turnLeft,
    thrust, 
) where

import Graphics.UI.GLUT (Key(..), SpecialKey(..))

turnLeft = SpecialKey KeyLeft
turnRight = SpecialKey KeyRight
thrust = SpecialKey KeyUp

