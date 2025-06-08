module Layers.Layer where

import Prelude

data DeckLayers
    = BaseLayer
    | BrowserLayer

    deriving stock (Bounded, Enum, Eq, Show)
