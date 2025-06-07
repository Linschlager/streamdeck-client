module Teletubbies where

import FRP.StreamDeck.DisplayButtonEvents
import FRP.StreamDeck.Layer
import Image
import Prelude

data Teletubbies = Red | Blue deriving stock (Bounded, Enum, Eq, Show)

instance Layer DisplayButtonEvent Teletubbies where
    layerEvent (DisplayButtonReleased 0) Blue =
        SwitchLayers
            { fromLayer = Blue
            , toLayer = Red
            }
    layerEvent (DisplayButtonPressed 0) Red =
        SwitchLayers
            { fromLayer = Red
            , toLayer = Blue
            }
    layerEvent event onLayer = LayerEvent{..}

handleLayerEvent
    :: forall s m
     . ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       )
    => LayerEvent DisplayButtonEvent Teletubbies
    -> StreamDeckT m s ()
handleLayerEvent
    LayerEvent
        { event = DisplayButtonPressed key
        , onLayer = Red
        } =
        setDisplayButtonImage key $ redDynamicKeyImage @s
handleLayerEvent
    LayerEvent
        { event = DisplayButtonPressed key
        , onLayer = Blue
        } =
        setDisplayButtonImage key $ blueDynamicKeyImage @s
handleLayerEvent
    LayerEvent
        { event = DisplayButtonReleased key
        } =
        setDisplayButtonImage key $ blackDynamicKeyImage @s
handleLayerEvent _ = pure ()
