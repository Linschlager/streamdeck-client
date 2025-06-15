{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Main where

import Debug.Trace
import FRP.Rhine
import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckMk2Clock
    ( StreamDeckMk2Clock (StreamDeckMk2Clock), StreamDeckMk2Event
    )
import FRP.StreamDeck.StreamDeckPlusClock (StreamDeckPlusClock(StreamDeckPlusClock))
import StreamDeckPlus qualified
import System.Hardware.StreamDeck qualified as StreamDeck
import System.Hardware.StreamDeck.StreamDeckMk2
import System.Hardware.StreamDeck.StreamDeckPlus
import Prelude
import Github.GithubClock
import Layers.Layer (DeckLayers (..), LayerUpdate(..), doSetup)
import Control.Monad.Schedule.Class
import System.Hardware.StreamDeck (StreamDeckT(..))
import StreamDeckMk2 qualified

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
