{-# OPTIONS_GHC -Wno-orphans #-}
module StreamDeckMk2 where

import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckMk2Clock
import Teletubbies
import Prelude

instance Layer StreamDeckMk2Event Teletubbies where
    layerEvent (DisplayButtonEvent e) l =
        case layerEvent e l of
            LayerEvent{..} -> LayerEvent{onLayer, event = DisplayButtonEvent event}
            SwitchLayers{..} -> SwitchLayers{..}

handleLayerEvent
    :: ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       )
    => LayerEvent StreamDeckMk2Event Teletubbies
    -> StreamDeckT m s ()
handleLayerEvent SwitchLayers{..} = Teletubbies.handleLayerEvent SwitchLayers{..}
handleLayerEvent LayerEvent{event = DisplayButtonEvent event, ..} = Teletubbies.handleLayerEvent LayerEvent{..}

deriving stock instance Show StreamDeckMk2Event
