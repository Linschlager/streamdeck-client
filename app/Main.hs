{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Main where

import Control.Monad.Schedule.Class
import Github.GithubClock
import Layers.Layer (DeckLayers (..), LayerUpdate(..), doSetup)
import Prelude
import StreamDeckMk2 qualified
import StreamDeckPlus qualified
import System.Hardware.StreamDeck (StreamDeckT(..))
import System.Hardware.StreamDeck qualified as StreamDeck

instance forall io s. (MonadSchedule io, Monad io) => MonadSchedule (StreamDeckT io s) where
  schedule as = StreamDeck . fmap (second (StreamDeck <$>)) $ schedule (unStreamDeck <$> as)
    where unStreamDeck (StreamDeck x) = x

main :: IO ()
main = do
    void $ StreamDeck.runStreamDeck @StreamDeckMk2 do
        traceShowM =<< asks (.deviceInfo)
        doSetup
        flow $
            (layer BaseLayer >-> arr ByLayerEvent @@ StreamDeckMk2Clock
            |@| tagS >-> arr ByGithub @@ GithubClock
            )
            @>-^ traceMSF "Event: "
            @>-^ arrMCl StreamDeckMk2.handleLayerUpdate
    void $ StreamDeck.runStreamDeck @StreamDeckPlus do
        traceShowM =<< asks (.deviceInfo)
        doSetup
        flow $ 
            (layer BaseLayer >-> arr ByLayerEvent @@ StreamDeckPlusClock
            |@|
            tagS >-> arr ByGithub @@ GithubClock
            ) @>-^ traceMSF "Event: "
            @>-^ arrMCl StreamDeckPlus.handleLayerUpdate
