{-# OPTIONS_GHC -Wno-orphans #-}
module StreamDeckPlus where

import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckPlusClock
import Layers.Layer (handleLayerEvent, DeckLayers)
import Prelude

instance Layer StreamDeckPlusEvent DeckLayers where
    layerEvent (DisplayButtonEvent e) l =
        case layerEvent e l of
            LayerEvent{..} -> LayerEvent{onLayer, event = DisplayButtonEvent event}
            SwitchLayers{..} -> SwitchLayers{..}
    layerEvent event onLayer = LayerEvent{..}

handleLayerEvent
    :: ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       )
    => LayerEvent StreamDeckPlusEvent DeckLayers
    -> StreamDeckT m s ()
handleLayerEvent SwitchLayers{..} = Layers.Layer.handleLayerEvent SwitchLayers{..}
handleLayerEvent LayerEvent{event = DisplayButtonEvent event, ..} = Layers.Layer.handleLayerEvent LayerEvent{..}
handleLayerEvent _ = pure ()

deriving stock instance Show StreamDeckPlusEvent
