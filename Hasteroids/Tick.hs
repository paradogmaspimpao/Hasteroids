module Hasteroids.Tick where

import Hasteroids.Keyboard (Keyboard)

class Tickable t where
    tick :: Keyboard -> t -> t

