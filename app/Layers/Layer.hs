module Layers.Layer where

import Prelude
import FRP.StreamDeck.Layer
import FRP.StreamDeck.DisplayButtonEvents
import FontToImage qualified
import SvgImage qualified
import FontToImage (TextAlignment(..))
import Image (setDisplayButtonImage)
import System.Hardware.StreamDeck qualified as StreamDeck
import Github.Types (PullRequest(..))

data DeckLayers
    = BaseLayer
    deriving stock (Bounded, Enum, Eq, Show)

data LayerUpdate e l
    = ByLayerEvent (LayerEvent e l)
    | ByGithub [PullRequest]
    deriving stock (Show)

-- | TODO switch layers
instance Layer DisplayButtonEvent DeckLayers where
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

updateGithubButtons :: forall m s. (MonadIO m, MonadFail m, IsStreamDeckWithDisplayButtons s) => [PullRequest] -> StreamDeckT m s ()
updateGithubButtons prs = do
    let githubStartIndex  = 0 :: Int
    font <- liftIO FontToImage.loadFont
    forM_ (zip [0..] prs) $ \((+ githubStartIndex) -> key, pr) ->
        setDisplayButtonImage key . SvgImage.drawImage @s $ FontToImage.textToImage @s font pr.title BottomLeft

doSetup :: forall m s.
       ( MonadIO m
       , MonadFail m
       , IsStreamDeckWithDisplayButtons s
       ) =>
    StreamDeckT m s ()
doSetup = do
    let x :: Int = StreamDeck.displayButtonCount @s
    font <- liftIO FontToImage.loadFont
    forM_ [0..x-1] $ \key ->
            Image.setDisplayButtonImage key
                $ SvgImage.drawImage @s
                $ FontToImage.textToImage @s font (show key) Center

