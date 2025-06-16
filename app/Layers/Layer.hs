module Layers.Layer where

import FRP.StreamDeck.DisplayButtonEvents
import FontToImage (TextAlignment(..))
import FontToImage qualified
import Github.Types (RepositoryResponse(..), PullRequest(..))
import Image (setDisplayButtonImage)
import Prelude
import SvgImage qualified
import System.Hardware.StreamDeck qualified as StreamDeck
import Data.Text qualified as Text

data LayerState = LayerState
    { currentLayer :: DeckLayer
    , github :: Maybe RepositoryResponse
    }

data DeckLayer
    = BaseLayer
    deriving stock (Bounded, Enum, Eq, Show)

data LayerUpdate e l
    = ByLayerEvent (LayerEvent e l)
    | ByGithub RepositoryResponse
    deriving stock (Show)

-- | TODO switch layers
instance Layer DisplayButtonEvent DeckLayer where
    layerEvent event onLayer = LayerEvent{..}

handleLayerEvent
    :: forall s m
     . ( MonadIO m
       )
    => LayerEvent DisplayButtonEvent DeckLayer
    -> LayerState
    -> StreamDeckT m s ()
handleLayerEvent
    LayerEvent { event = DisplayButtonPressed key, onLayer = BaseLayer } LayerState { github = Just gh } = do
        case gh.pullRequests !? key of
            Just pr -> do
                liftIO . flip cmd "" $ "xdg-open" :| [ Text.pack pr.url ]
                pure ()
            Nothing -> pure ()
handleLayerEvent _ _ = pure ()

-- | TODO
-- 1. Filter out old reviews.
-- 2. Show CTA per PR: Fully Approved = GREEN, Merge Conflicts = RED, NeedToReview = ORANGE, reviews received but not handled = YELLOW, no action needed = GRAY
-- 3. On Press: Open PR in Browser
-- ... Look into WebHooks
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

