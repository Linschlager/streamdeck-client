module Layers.Layer where

import Prelude
import FRP.StreamDeck.Layer
import FRP.StreamDeck.DisplayButtonEvents
import FontToImage qualified
import SvgImage qualified
import FontToImage (TextAlignment(..))
import Image (setDisplayButtonImage)

data DeckLayers
    = BaseLayer
    | BrowserLayer

    deriving stock (Bounded, Enum, Eq, Show)

instance Layer DisplayButtonEvent DeckLayers where
    layerEvent (DisplayButtonReleased 0) BrowserLayer =
        SwitchLayers
            { fromLayer = BrowserLayer
            , toLayer = BaseLayer
            }
    layerEvent (DisplayButtonPressed 0) BaseLayer =
        SwitchLayers
            { fromLayer = BaseLayer
            , toLayer = BrowserLayer
            }
    layerEvent event onLayer = LayerEvent{..}

handleLayerEvent
    :: forall s m
     . ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       )
    => LayerEvent DisplayButtonEvent DeckLayers
    -> StreamDeckT m s ()
handleLayerEvent
    LayerEvent
        { event = DisplayButtonPressed key
        , onLayer = BaseLayer
        } = do
        font <- liftIO FontToImage.loadFont
        setDisplayButtonImage key . SvgImage.drawImage @s $ FontToImage.textToImage @s font (show key) BottomCenter
handleLayerEvent _ = pure ()
