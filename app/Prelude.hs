module Prelude
    ( module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader.Class
    , module Data.Aeson
    , module Data.Bits
    , module Data.ByteString
    , module Data.ByteString.Lazy
    , module Data.Either
    , module Control.Applicative
    , module Data.Maybe
    , module Data.Ord
    , module Data.Text
    , module Data.List
    , module Data.List.NonEmpty
    , module Data.Word
    , module Debug.Trace
    , module FRP.Rhine
    , module FRP.StreamDeck.Layer
    , module FRP.StreamDeck.StreamDeckMk2Clock
    , module FRP.StreamDeck.StreamDeckPlusClock 
    , module System.Hardware.StreamDeck.StreamDeckMk2
    , module System.Hardware.StreamDeck.StreamDeckPlus
    , module Codec.Picture
    , module Graphics.Rasterific
    , module GHC.Generics
    , module Prelude
    , module System.Hardware.StreamDeck
    )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Maybe (catMaybes, fromMaybe, maybe, fromJust)
import Data.Either (fromRight)
import Data.Ord
import Data.List ((!?))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Data.Word (Word16, Word8)
import Debug.Trace
import FRP.Rhine hiding (forever, newChan, try, Result)
import FRP.StreamDeck.Layer
import FRP.StreamDeck.StreamDeckMk2Clock hiding (DisplayButtonEvent)
import FRP.StreamDeck.StreamDeckPlusClock hiding (DisplayButtonEvent)
import System.Hardware.StreamDeck.StreamDeckMk2
import System.Hardware.StreamDeck.StreamDeckPlus
import GHC.Generics
import Graphics.Rasterific (Drawing)
import Codec.Picture (PixelRGBA8)
import System.Hardware.StreamDeck
    ( IsStreamDeck
    , IsStreamDeckWithButtons
    , IsStreamDeckWithDisplayButtons
    , IsStreamDeckWithKnobs
    , StreamDeckT
    )
import "base" Prelude
import Control.Applicative ((<|>))
import System.Process.Typed (StreamSpec, ExitCode (ExitSuccess))
import System.Process.Typed qualified as Process
import Data.Text qualified as Text
import System.Exit (exitWith)

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

cmd'
    :: NonEmpty Text
    -> StreamSpec 'Process.STInput ()
    -> IO (ExitCode, LazyByteString, LazyByteString)
cmd' (x :| xs) input = do
    (exitCode, out, err) <-
        Process.readProcess . Process.setStdin input $
            Process.proc (Text.unpack x) (Text.unpack <$> xs)
    pure (exitCode, out, err)

cmd :: NonEmpty Text -> StreamSpec 'Process.STInput () -> IO LazyByteString
cmd xs input =
    cmd' xs input >>= \case
        (ExitSuccess, out, _) -> pure out
        (code, _, _) -> exitWith code

cmd_ :: NonEmpty Text -> StreamSpec 'Process.STInput () -> IO ()
cmd_ = (void .) . cmd
