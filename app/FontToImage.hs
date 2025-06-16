module FontToImage where

import Codec.Picture.Types
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Text.TrueType (Font, loadFontFile, stringBoundingBox, BoundingBox (..))
import Prelude
import System.Hardware.StreamDeck qualified as StreamDeck

data TextAlignment
    = TopLeft
    | TopCenter
    | TopRight
    | CenterLeft
    | Center
    | CenterRight
    | BottomLeft
    | BottomCenter
    | BottomRight

alignmentAnchor :: forall s. IsStreamDeckWithDisplayButtons s => BoundingBox -> TextAlignment -> Point
alignmentAnchor BoundingBox{..} alignment = do
    let bWidth = StreamDeck.buttonImageWidth @s
    let bHeight = StreamDeck.buttonImageHeight @s
    let padding = 4 :: Float

    let width = _xMax - _xMin
    let height = _yMax - _yMin
    let rightX = fromIntegral bWidth - width
    let bottomY = fromIntegral bHeight
    let centeredX = fromIntegral bWidth / 2 - width / 2
    let centeredY = fromIntegral bHeight / 2 + height / 2

    case alignment of
       TopLeft -> V2 0 height + V2 padding padding
       TopCenter -> V2 centeredX height + V2 0 padding
       TopRight -> V2 rightX height + V2 (-padding) padding
       CenterLeft -> V2 0 centeredY + V2 padding 0
       Center -> V2 centeredX centeredY
       CenterRight -> V2 rightX centeredY + V2 (-padding) 0
       BottomLeft -> V2 0 bottomY + V2 padding (-padding)
       BottomCenter -> V2 centeredX bottomY + V2 0 (-padding)
       BottomRight -> V2 rightX bottomY + V2 (-padding) (-padding)


loadFont :: IO Font
loadFont = fromRight (error "Could not load font") <$> loadFontFile "fonts/BarlowCondensed-Regular.ttf"

fontAlignment :: forall s. IsStreamDeckWithDisplayButtons s => Font -> PointSize -> String -> TextAlignment -> Point
fontAlignment font fontSize inputText align = do
    -- Default DPI as defined in Rasterific
    let dpi = 96 :: Int
    let bb = stringBoundingBox font dpi fontSize inputText
    alignmentAnchor @s bb align


textToImage :: forall s. IsStreamDeckWithDisplayButtons s => Font -> String -> TextAlignment -> Drawing PixelRGBA8 ()
textToImage font inputText align = do
    let bHeight = StreamDeck.buttonImageHeight @s
    let fontSize = PointSize (fromIntegral bHeight / 9)
    let drawColor = PixelRGBA8 0 0 0 255
    let fontAnchor = fontAlignment @s font fontSize inputText align

    withTexture (uniformTexture drawColor) $ do
        printTextAt font fontSize fontAnchor inputText

