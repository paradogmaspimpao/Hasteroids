module Hasteroids.Tick where -- Reverted

import Hasteroids.Keyboard (Keyboard) -- Reverted

class Tickable t where
    tick :: Keyboard -> t -> t
