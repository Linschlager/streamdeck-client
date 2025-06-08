{-# OPTIONS_GHC -Wno-orphans #-}
module StreamDeckPlus where

import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckPlusClock
import Layers.Layer (handleLayerEvent, DeckLayers)
import System.Hardware.StreamDeck qualified as StreamDeck
import Image qualified
import SvgImage qualified
import FontToImage qualified
import FontToImage (TextAlignment(..))
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

doSetup :: forall m s.
       ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       ) =>
    StreamDeckT m s ()
doSetup = do
    let x :: Int = StreamDeck.displayButtonCount @s
    font <- liftIO FontToImage.loadFont
    forM_ [0..x-1] $ \key -> do
        Image.setDisplayButtonImage key 
            $ SvgImage.drawImage @s
            $ FontToImage.textToImage @s font (show key) Center


deriving stock instance Show StreamDeckPlusEvent
