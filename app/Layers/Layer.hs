module Layers.Layer where

import Prelude
import FRP.StreamDeck.Layer
import FRP.StreamDeck.DisplayButtonEvents
import FontToImage qualified
import SvgImage qualified
import FontToImage (TextAlignment(..))
import Image (setDisplayButtonImage)
import System.Hardware.StreamDeck qualified as StreamDeck
import Buttons.GithubPrButton qualified as GithubPrButton
import Actions.GithubPrStatus qualified as GithubPrStatus

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

doSetup :: forall m s.
       ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       ) =>
    StreamDeckT m s ()
doSetup = do
    let x :: Int = StreamDeck.displayButtonCount @s
    font <- liftIO FontToImage.loadFont
    prs <- liftIO GithubPrStatus.pullRequestsForRepo
    forM_ [0..x-1] $ \key -> case key of
        ((prs !?) -> Just pr) -> 
            Image.setDisplayButtonImage key
                . SvgImage.drawImage @s
                =<< GithubPrButton.update @s pr
        _ -> 
            Image.setDisplayButtonImage key
                $ SvgImage.drawImage @s
                $ FontToImage.textToImage @s font (show key) Center
