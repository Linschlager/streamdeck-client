{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Debug.Trace
import FRP.Rhine
import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckMk2Clock
    ( StreamDeckMk2Clock (StreamDeckMk2Clock)
    )
import StreamDeckMk2 qualified
import System.Hardware.StreamDeck qualified as StreamDeck
import System.Hardware.StreamDeck.StreamDeckMk2
import Prelude
import Layers.Layer (DeckLayers (..))
import Control.Monad.Schedule.Class
import System.Hardware.StreamDeck (StreamDeckT(..))

instance forall io s. (MonadSchedule io, Monad io) => MonadSchedule (StreamDeckT io s) where
  schedule as = StreamDeck . fmap (second (StreamDeck <$>)) $ schedule (unStreamDeck <$> as)
    where unStreamDeck (StreamDeck x) = x

tick
    :: forall io m s
    . (MonadIO io
      , m ~ StreamDeckT io s
      )
    => ClSF m (IOClock m (Millisecond 1000)) () ()
tick =
    arr timeInfoOf sinceInit >-> arr show >-> arr ("Click has ticked at " ++) >-> arrMCl (liftIO . print)

foo
    :: forall cl io s m
     . ( MonadIO io
       , m ~ StreamDeckT io s
       , Show (Tag cl)
       , Show (Diff (Time cl))
       , Layer (Tag cl) DeckLayers
       )
    => (LayerEvent (Tag cl) DeckLayers -> m ())
    -> ClSF m cl () ()
foo handleEvent =
    layer BaseLayer
        >-> traceMSF "Event: "
        >-> arrMCl handleEvent

main :: IO ()
main = do
    void $ StreamDeck.runStreamDeck @StreamDeckMk2 do
        traceShowM =<< asks (.deviceInfo)
        StreamDeckMk2.doSetup
        flow $
            foo StreamDeckMk2.handleLayerEvent @@ StreamDeckMk2Clock
            |@|
            tick @@ ioClock waitClock
    --void $ StreamDeck.runStreamDeck @StreamDeckPlus do
    --    traceShowM =<< asks (.deviceInfo)
    --    StreamDeckPlus.doSetup
    --    flow $ foo StreamDeckPlus.handleLayerEvent @@ StreamDeckPlusClock
