module Main
  ( main
  ) where

import           Brick                      (App (..), BrickEvent (..), EventM,
                                             Next, Widget, attrMap, continue,
                                             customMain, halt, neverShowCursor,
                                             str, withBorderStyle, (<+>))
import           Brick.BChan                (newBChan, writeBChan)
import           Brick.Widgets.Border       (borderWithLabel, vBorder)
import           Brick.Widgets.Border.Style (ascii)
import           Brick.Widgets.Center       (center)
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               (preview, view)
import           Control.Monad              (forever, void)
import           Data.Aeson.Lens            (AsValue, key, _Number, _String)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text, unpack)
import qualified Graphics.Vty               as V
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

drawUI :: Forecast -> [Widget Name]
drawUI (Forecast latLng temp summary) =
  pure $
  withBorderStyle ascii $
  borderWithLabel (title latLng) $
  center (str summary) <+> vBorder <+> center (str (show temp <> "°C"))

type Name = ()

newtype Event =
  ReceiveForecastSuccess Forecast

handleEvent :: Forecast -> BrickEvent Name Event -> EventM Name (Next Forecast)
handleEvent _ (AppEvent (ReceiveForecastSuccess next))     = continue next
handleEvent previous (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt previous
handleEvent previous (VtyEvent (V.EvKey V.KEsc []))        = halt previous
handleEvent previous _                                     = continue previous

app :: App Forecast Event Name
app =
  App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const (attrMap V.defAttr [])
  }

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
  chan <- newBChan 10
  void
    (forkIO
       (forever
          ((writeBChan chan =<<
            ReceiveForecastSuccess . extractForecast work <$>
            getForecast apiKey work) >>
           threadDelay 30000000)))
  void (customMain (V.mkVty V.defaultConfig) (Just chan) app (Forecast "" 0 ""))
