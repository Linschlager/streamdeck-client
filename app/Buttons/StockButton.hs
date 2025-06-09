module Buttons.StockButton where

import Prelude
import SvgImage qualified
import FontToImage qualified
import Actions.LookupStocks qualified as LookupStocks
import FontToImage (TextAlignment (..))
import Codec.Picture (PixelRGBA8 (..))
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import System.Hardware.StreamDeck qualified as StreamDeck

percentageTriangle :: forall s. IsStreamDeckWithDisplayButtons s => Float -> Drawing PixelRGBA8 ()
percentageTriangle deltaPct = do
    let texture = if deltaPct > 0 then PixelRGBA8 0 255 0 255 else PixelRGBA8 255 0 0 255
    let height = fromIntegral $ StreamDeck.buttonImageHeight @s
    withTexture (uniformTexture texture) $
        fill $ circle (V2 15 (height / 2)) 5

update :: forall s. IsStreamDeckWithDisplayButtons s => String -> IO (Drawing PixelRGBA8 ())
update symbol = do
    font <- FontToImage.loadFont
    info <- LookupStocks.requestStockPrice symbol
    pure $ 
        SvgImage.mergeImage
            (percentageTriangle @s info.changePercent) $
            SvgImage.mergeImage
                (FontToImage.textToImage @s font (show info.price) Center)
                (FontToImage.textToImage @s font info.symbol TopCenter)
