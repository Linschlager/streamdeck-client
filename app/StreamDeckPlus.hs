module StreamDeckPlus where

import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckPlusClock
import Teletubbies
import Prelude

instance Layer StreamDeckPlusEvent Teletubbies where
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
    => LayerEvent StreamDeckPlusEvent Teletubbies
    -> StreamDeckT m s ()
handleLayerEvent SwitchLayers{..} = Teletubbies.handleLayerEvent SwitchLayers{..}
handleLayerEvent LayerEvent{event = DisplayButtonEvent event, ..} = Teletubbies.handleLayerEvent LayerEvent{..}
handleLayerEvent _ = pure ()

deriving stock instance Show StreamDeckPlusEvent
