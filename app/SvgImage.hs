module SvgImage where

import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType (Font, loadFontFile)
import Image hiding (red)
import Prelude
import System.Hardware.StreamDeck qualified as StreamDeck
import Data.Either (fromRight)

loadFont :: IO Font
loadFont = fromRight (error "Could not load font") <$> loadFontFile "fonts/BarlowCondensed-Regular.ttf"

mergeImage :: Drawing PixelRGBA8 () -> Drawing PixelRGBA8 () -> Drawing PixelRGBA8 ()
mergeImage img1 img2 =
    withTexture (uniformTexture (PixelRGBA8 255 255 255 255)) $ do 
        img1
        img2

textToImage :: Font -> String -> Drawing PixelRGBA8 ()
textToImage font inputText =
    let drawColor = PixelRGBA8 0 0 0 255
    in printTextRanges (V2 4 20) [TextRange font  (PointSize 16) inputText (Just $ uniformTexture drawColor)]

imageFromText :: forall s. IsStreamDeckWithDisplayButtons s => Font ->  String -> DynamicImage
imageFromText font inputText = do
    let white = PixelRGBA8 255 255 255 255
    let red = PixelRGBA8 255 0 0 255

    let width = StreamDeck.buttonImageWidth @s
    let height = StreamDeck.buttonImageHeight @s

    let fillImg = 
         withTexture (uniformTexture red) $
            fill $ rectangle (V2 0 0) (fromIntegral width) (fromIntegral height)


    let img :: Image PixelRGBA8
        img = renderDrawing width height white $
            mergeImage fillImg $
                textToImage font inputText
    imageToDynamic img
