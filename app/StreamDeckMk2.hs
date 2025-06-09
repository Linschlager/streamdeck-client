{-# OPTIONS_GHC -Wno-orphans #-}
module StreamDeckMk2 where

import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckMk2Clock
import Layers.Layer (handleLayerEvent, DeckLayers(..))
import Prelude

instance Layer StreamDeckMk2Event DeckLayers where
    layerEvent (DisplayButtonEvent e) l =
        case layerEvent e l of
            LayerEvent{..} -> LayerEvent{onLayer, event = DisplayButtonEvent event}
            SwitchLayers{..} -> SwitchLayers{..}

handleLayerEvent
    :: ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       )
    => LayerEvent StreamDeckMk2Event DeckLayers
    -> StreamDeckT m s ()
handleLayerEvent SwitchLayers{..} = Layers.Layer.handleLayerEvent SwitchLayers{..}
handleLayerEvent LayerEvent{event = DisplayButtonEvent event, ..} = Layers.Layer.handleLayerEvent LayerEvent{..}

deriving stock instance Show StreamDeckMk2Event
