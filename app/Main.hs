{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Main where

import Control.Monad.Schedule.Class
import Github.GithubClock
import Layers.Layer -- (DeckLayer (..), LayerUpdate(..), LayerState(..), doSetup)
import Prelude
import StreamDeckMk2 qualified
import StreamDeckPlus qualified
import System.Hardware.StreamDeck (StreamDeckT(..))
import System.Hardware.StreamDeck qualified as StreamDeck

instance forall io s. (MonadSchedule io, Monad io) => MonadSchedule (StreamDeckT io s) where
  schedule as = StreamDeck . fmap (second (StreamDeck <$>)) $ schedule (unStreamDeck <$> as)
    where unStreamDeck (StreamDeck x) = x

statefulSink :: (MonadIO m, MonadFail m) => (LayerUpdate StreamDeckMk2Event DeckLayer, LayerState) -> StreamDeckT m StreamDeckMk2 ((), LayerState)
statefulSink (u, st) = do
    let newSt = st
    StreamDeckMk2.handleLayerUpdate u
    pure ((), newSt)

streamdeckRhine :: Rhine (StreamDeckT IO StreamDeckMk2) StreamDeckMk2Clock () (LayerUpdate StreamDeckMk2Event DeckLayer)
streamdeckRhine = layer BaseLayer >-> arr ByLayerEvent @@ StreamDeckMk2Clock

githubRhine :: Rhine (StreamDeckT IO StreamDeckMk2) GithubClock () (LayerUpdate e l)
githubRhine = tagS >-> arr ByGithub @@ GithubClock

main :: IO ()
main = void . StreamDeck.runStreamDeck @StreamDeckMk2 @IO $ do
        traceShowM =<< asks (.deviceInfo)
        doSetup
        let initialState = LayerState { currentLayer = BaseLayer, github = Nothing }
        flow $ (streamdeckRhine |@| githubRhine) @>-^ feedback initialState (arrMCl statefulSink)
