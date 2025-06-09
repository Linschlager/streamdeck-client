module Layers.Layer where

import Prelude
import FRP.StreamDeck.Layer
import FRP.StreamDeck.DisplayButtonEvents
import FontToImage qualified
import SvgImage qualified
import FontToImage (TextAlignment(..))
import Image (setDisplayButtonImage)
import System.Hardware.StreamDeck qualified as StreamDeck
import Buttons.StockButton qualified as StockButton

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

setStockImage :: forall m s. (MonadIO m, MonadFail m, IsStreamDeckWithDisplayButtons s) => Int -> String -> StreamDeckT m s ()
setStockImage key symbol = do
    image <- liftIO $ StockButton.update @s symbol
    Image.setDisplayButtonImage key $
        SvgImage.drawImage @s image

maybeStockButton :: Int -> Maybe String
maybeStockButton key =
    case key of
        1 -> Just "TSLA"
        2 -> Just "AAPL"
        3 -> Just "NVDA"
        4 -> Just "SMI"
        _ -> Nothing

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
        { event = DisplayButtonPressed key@(maybeStockButton -> Just symbol)
        , onLayer = BaseLayer
        } = do
        setStockImage key symbol
handleLayerEvent
    LayerEvent
        { event = DisplayButtonPressed key
        , onLayer = BaseLayer
        } = do
        font <- liftIO FontToImage.loadFont
        setDisplayButtonImage key . SvgImage.drawImage @s $ FontToImage.textToImage @s font (show key) BottomCenter
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
    forM_ [0..x-1] $ \key -> case key of
        (maybeStockButton -> Just symbol) -> setStockImage key symbol
        _ -> 
            Image.setDisplayButtonImage key
                $ SvgImage.drawImage @s
                $ FontToImage.textToImage @s font (show key) Center
