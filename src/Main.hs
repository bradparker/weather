module Main
  ( main
  ) where

import           Brick                      (Widget, simpleMain, str,
                                             withBorderStyle, (<+>))
import           Brick.Widgets.Border       (borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (center)
import           Control.Lens               (preview, view)
import           Data.Aeson.Lens            (AsValue, key, _Number, _String)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text, unpack)
import           Network.Wreq               (Response, get, responseBody)
import           System.Environment         (getEnv)

work :: String
work = "-27.464915,153.028417"

darkSky :: String
darkSky = "https://api.darksky.net"

(</>) :: String -> String -> String
a </> b = a <> "/" <> b

(<?>) :: String -> String -> String
a <?> b = a <> "?" <> b

type Field f a b = (a -> f a) -> Response b -> f (Response b)

extractTemp :: (Applicative f, AsValue body) => Field f Scientific body
extractTemp = responseBody . key "currently" . key "temperature" . _Number

extractSummary :: (Applicative f, AsValue body) => Field f Text body
extractSummary = responseBody . key "currently" . key "summary" . _String

getForecast :: String -> String -> IO (Response ByteString)
getForecast apiKey latLng =
  get (darkSky </> "forecast" </> apiKey </> latLng <?> "units=si")

title :: String -> Widget ()
title latLng = str ("Weather at " <> latLng)

ui :: Forecast -> Widget ()
ui (Forecast latLng temp summary) =
  withBorderStyle unicode $
  borderWithLabel (title latLng) $
  center (str summary) <+> vBorder <+> center (str (show temp <> "°C"))

data Forecast =
  Forecast String
           Scientific
           String

extractForecast :: String -> Response ByteString -> Forecast
extractForecast latLng =
  Forecast <$> pure latLng <*> fromMaybe 0 . preview extractTemp <*>
  unpack . view extractSummary

main :: IO ()
main = do
  apiKey <- getEnv "DARK_SKY_API_KEY"
  forecast <- extractForecast work <$> getForecast apiKey work
  simpleMain (ui forecast)
