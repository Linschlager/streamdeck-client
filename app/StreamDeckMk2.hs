{-# OPTIONS_GHC -Wno-orphans #-}
module StreamDeckMk2 where

import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckMk2Clock
import Layers.Layer (handleLayerEvent, DeckLayers(..))
import Prelude
import System.Hardware.StreamDeck qualified as StreamDeck
import Image qualified
import FontToImage qualified
import FontToImage (TextAlignment(..))
import SvgImage qualified

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

deriving stock instance Show StreamDeckMk2Event
