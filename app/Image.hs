module Image where

import Codec.Picture
import Codec.Picture.Extra (flipHorizontally, flipVertically)
import Codec.Picture.Types
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import System.Hardware.StreamDeck qualified as StreamDeck
import Prelude

encodeImage :: (ColorSpaceConvertible px PixelYCbCr8) => Image px -> ByteString
encodeImage =
    LBS.toStrict
        . encodeJpegAtQuality 100
        . convertImage
        . flipHorizontally
        . flipVertically

encodeDynamicImage :: DynamicImage -> ByteString
encodeDynamicImage = encodeImage . convertRGB8

generateKeyImage
    :: forall px s
     . ( Pixel px
       , IsStreamDeckWithDisplayButtons s
       )
    => (Int -> Int -> px)
    -> Image px
generateKeyImage f =
    generateImage
        f
        (StreamDeck.buttonImageWidth @s)
        (StreamDeck.buttonImageHeight @s)

imageToDynamic :: (BmpEncodable px) => Image px -> DynamicImage
imageToDynamic =
    fromRight (error "generateDynamicKeyImage")
        . decodeBitmap
        . LBS.toStrict
        . encodeBitmap

generateDynamicKeyImage
    :: forall px s
     . (Pixel px, BmpEncodable px, IsStreamDeckWithDisplayButtons s)
    => (Int -> Int -> px)
    -> DynamicImage
generateDynamicKeyImage = imageToDynamic . generateKeyImage @px @s

setDisplayButtonImage
    :: forall m s
     . ( MonadFail m
       , MonadIO m
       , IsStreamDeckWithDisplayButtons s
       )
    => Int
    -> DynamicImage
    -> StreamDeckT m s ()
setDisplayButtonImage key (encodeDynamicImage -> image) = StreamDeck.setButtonImage key image

black :: PixelRGB8
black = PixelRGB8 0 0 0

blackDynamicKeyImage
    :: forall s
     . (IsStreamDeckWithDisplayButtons s)
    => DynamicImage
blackDynamicKeyImage = generateDynamicKeyImage @_ @s . const . const $ black

red :: PixelRGB8
red = PixelRGB8 255 0 0

redDynamicKeyImage
    :: forall s
     . (IsStreamDeckWithDisplayButtons s)
    => DynamicImage
redDynamicKeyImage = generateDynamicKeyImage @_ @s . const . const $ red

blue :: PixelRGB8
blue = PixelRGB8 0 0 255

blueDynamicKeyImage
    :: forall s
     . (IsStreamDeckWithDisplayButtons s)
    => DynamicImage
blueDynamicKeyImage = generateDynamicKeyImage @_ @s . const . const $ blue
