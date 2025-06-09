{-# OPTIONS_GHC -Wno-operator-whitespace #-}
module Actions.LookupStocks where

import System.Environment
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (fromJust)
import Prelude
import Network.HTTP.Client (httpLbs, parseRequest, Response (responseBody))
import Network.HTTP.Client.TLS (newTlsManager)

data StockInfo = StockInfo
    { symbol :: String
    , price :: Float
    , changePercent :: Float
    }

instance FromJSON StockInfo where
    parseJSON =
        withObject "StockInfo" $ \d ->

            flip (withObject "GlobalQuote") (fromJust $ KeyMap.lookup "Global Quote" d) $ \i ->
                StockInfo
                    <$> i .: "01. symbol"
                    <*> ((\x -> trace x (read x)) <$> i .: "05. price")
                    <*> (read . init <$> i .: "10. change percent")

url :: String -> String -> String
url symbol apiKey = "https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol="++symbol++"&apikey="++apiKey

apiKeyFromEnv :: IO String
apiKeyFromEnv = getEnv "AV_APIKEY"

requestStockPrice :: String -> IO StockInfo
requestStockPrice symbol = do
    apiKey <- apiKeyFromEnv
    let u = url symbol apiKey
    mgr <- newTlsManager
    req <- parseRequest u
    lbu <- httpLbs req mgr
    either fail pure $ eitherDecode $ responseBody lbu

