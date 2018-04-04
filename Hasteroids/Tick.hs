module Hasteroids.Tick where

class Tickable t where
    tick :: t -> t

