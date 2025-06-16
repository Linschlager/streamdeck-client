{-# OPTIONS_GHC -Wno-orphans #-}
module StreamDeckPlus where

import FRP.StreamDeck.StreamDeckPlusClock (StreamDeckPlusEvent(..))
import Layers.Layer
import Prelude
import Github.Types

instance Layer StreamDeckPlusEvent DeckLayer where
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
    => LayerUpdate StreamDeckPlusEvent DeckLayer
    -> LayerState
    -> StreamDeckT m s ()
handleLayerUpdate (ByLayerEvent (SwitchLayers{..})) state = handleLayerEvent SwitchLayers{..} state
handleLayerUpdate (ByLayerEvent (LayerEvent {event = DisplayButtonEvent event, onLayer})) state = handleLayerEvent LayerEvent{..} state
handleLayerUpdate (ByLayerEvent (LayerEvent {event = KnobEvent _})) state = undefined
handleLayerUpdate (ByLayerEvent (LayerEvent {event = TouchScreenEvent} ))  state = undefined
handleLayerUpdate (ByGithub prs) state = updateGithubButtons prs.pullRequests

deriving stock instance Show StreamDeckPlusEvent
