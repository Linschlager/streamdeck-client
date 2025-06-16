{-# OPTIONS_GHC -Wno-orphans #-}
module StreamDeckMk2 where

import Layers.Layer
import Prelude
import Github.Types
import FRP.StreamDeck.StreamDeckMk2Clock (StreamDeckMk2Event(..))

instance Layer StreamDeckMk2Event DeckLayer where
    layerEvent (DisplayButtonEvent e) l =
        case layerEvent e l of
            LayerEvent{..} -> LayerEvent{onLayer, event = DisplayButtonEvent event}
            SwitchLayers{..} -> SwitchLayers{..}

handleLayerUpdate
    :: ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       )
    => LayerUpdate StreamDeckMk2Event DeckLayer
    -> LayerState
    -> StreamDeckT m s ()
handleLayerUpdate (ByLayerEvent (SwitchLayers{..})) state =
    handleLayerEvent SwitchLayers{..} state
handleLayerUpdate (ByLayerEvent LayerEvent {event = DisplayButtonEvent event, onLayer}) state =
    handleLayerEvent LayerEvent{..} state
handleLayerUpdate (ByGithub prs) state =
    updateGithubButtons prs.pullRequests

deriving stock instance Show StreamDeckMk2Event
