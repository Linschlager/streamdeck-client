module SvgImage where

import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture (uniformTexture)
import Image hiding (red)
import Prelude
import System.Hardware.StreamDeck qualified as StreamDeck

mergeImage :: Drawing PixelRGBA8 () -> Drawing PixelRGBA8 () -> Drawing PixelRGBA8 ()
mergeImage img1 img2 =
    withTexture (uniformTexture (PixelRGBA8 255 255 255 255)) $ do 
        img1
        img2

drawImage :: forall s. IsStreamDeckWithDisplayButtons s => Drawing PixelRGBA8 () -> DynamicImage
drawImage image = do
    let white = PixelRGBA8 255 255 255 255

    let width = StreamDeck.buttonImageWidth @s
    let height = StreamDeck.buttonImageHeight @s

    let img = renderDrawing width height white image
    imageToDynamic img
