module Main where

import Debug.Trace
import FRP.Rhine
import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckMk2Clock
    ( StreamDeckMk2Clock (StreamDeckMk2Clock)
    )
import FRP.StreamDeck.StreamDeckPlusClock
    ( StreamDeckPlusClock (StreamDeckPlusClock)
    )
import StreamDeckMk2 qualified
import StreamDeckPlus qualified
import System.Hardware.StreamDeck qualified as StreamDeck
import System.Hardware.StreamDeck.StreamDeckMk2
import System.Hardware.StreamDeck.StreamDeckPlus
import Teletubbies
import Prelude

foo
    :: forall cl io s m
     . ( MonadIO io
       , m ~ StreamDeckT io s
       , Show (Tag cl)
       , Show (Diff (Time cl))
       , Layer (Tag cl) Teletubbies
       )
    => (LayerEvent (Tag cl) Teletubbies -> m ())
    -> ClSF m cl () ()
foo handleEvent =
    layer Red
        >-> traceMSF "Event: "
        >-> arrMCl handleEvent

main :: IO ()
main = do
    void $ StreamDeck.runStreamDeck @StreamDeckMk2 do
        traceShowM =<< asks (.deviceInfo)
        flow $ foo StreamDeckMk2.handleLayerEvent @@ StreamDeckMk2Clock
    void $ StreamDeck.runStreamDeck @StreamDeckPlus do
        traceShowM =<< asks (.deviceInfo)
        flow $ foo StreamDeckPlus.handleLayerEvent @@ StreamDeckPlusClock
