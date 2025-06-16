{-# OPTIONS_GHC -Wno-orphans #-}
module StreamDeckPlus where

import FRP.StreamDeck.StreamDeckPlusClock (StreamDeckPlusEvent(..))
import Layers.Layer
import Prelude

instance Layer StreamDeckPlusEvent DeckLayers where
    layerEvent (DisplayButtonEvent e) l =
        case layerEvent e l of
            LayerEvent{..} -> LayerEvent{onLayer, event = DisplayButtonEvent event}
            SwitchLayers{..} -> SwitchLayers{..}
    layerEvent event onLayer = LayerEvent{..}

handleLayerUpdate
    :: ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       )
    => LayerUpdate StreamDeckPlusEvent DeckLayers
    -> StreamDeckT m s ()
handleLayerUpdate (ByLayerEvent (SwitchLayers{..})) = handleLayerEvent SwitchLayers{..}
handleLayerUpdate (ByLayerEvent (LayerEvent {event = DisplayButtonEvent event, onLayer})) = handleLayerEvent LayerEvent{..}
handleLayerUpdate (ByLayerEvent (LayerEvent {event = KnobEvent _})) = undefined
handleLayerUpdate (ByLayerEvent (LayerEvent {event = TouchScreenEvent} )) = undefined
handleLayerUpdate (ByGithub prs) = updateGithubButtons prs

deriving stock instance Show StreamDeckPlusEvent
