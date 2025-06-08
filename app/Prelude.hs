module Prelude
    ( module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader.Class
    , module Data.Bits
    , module Data.ByteString
    , module Data.Maybe
    , module Data.Ord
    , module Data.Word
    , module Data.Text
    , module Debug.Trace
    , module FRP.Rhine
    , module GHC.Generics
    , module Prelude
    , module System.Hardware.StreamDeck
    )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord
import Data.Text (Text)
import Data.Word (Word16, Word8)
import Debug.Trace
import FRP.Rhine hiding (forever, newChan, try)
import GHC.Generics
import System.Hardware.StreamDeck
    ( IsStreamDeck
    , IsStreamDeckWithButtons
    , IsStreamDeckWithDisplayButtons
    , IsStreamDeckWithKnobs
    , StreamDeckT
    )
import "base" Prelude


traceMSF
    :: forall a m t
     . (Show a, Monad m, Show (Diff (Time t)))
    => String
    -> ClSF m t a a
traceMSF prefix = proc a -> do
    t <- sinceInitS -< ()
    arrMCl traceM -< logStr t a
    returnA -< a
  where
    logStr :: Diff (Time t) -> a -> String
    logStr t x = concat ["[", show t, "] ", prefix, show x]
