{-# LANGUAGE OverloadedStrings #-}

module Plugins.Monitors.OpenWeatherMap where

import Plugins.Monitors.Common
import Network.HTTP.Simple
import Data.Aeson
import Data.Vector (headM)

weatherConfig :: IO MConfig
weatherConfig = mkMConfig
       "<name>: <temp>C, humidity <humidity>%" -- template
       [ "name"                                -- available replacements
       , "weatherDesc"
       , "temp"
       , "tempMin"
       , "tempMax"
       , "humidity"
       , "pressure"
       , "windSpeed"
       , "windDeg"
       ]

data WeatherInfo =
    WI { stationName :: String
       , weatherDesc :: String
       , temp        :: Float
       , tempMin     :: Float
       , tempMax     :: Float
       , humidity    :: Int
       , pressure    :: Float
       , windSpeed   :: Float
       , windDeg     :: Float
       } deriving (Show)

instance FromJSON WeatherInfo where
  parseJSON = withObject "WeatherInfo" $ \v -> do
    n <- v .: "name"
    des <- v .: "weather" >>=
      withArray "weather" headM >>=
      withObject "first" (.: "description")
    (t,tmin,tmax,hu,pr) <- v .: "main" >>=
      withObject "main" (\o -> (,,,,)
                          <$> o .: "temp" <*> o .: "temp_min"
                          <*> o .: "temp_max" <*> o .: "humidity"
                          <*> o .: "pressure")
    (ws, wd) <- v .: "wind" >>=
      withObject "wind" (\o -> (,) <$> o .: "speed" <*> o .: "deg")
    return $ WI n des t tmin tmax hu pr ws wd

defUrl, units :: String
defUrl = "https://api.openweathermap.org/data/2.5/weather?q="
units = "metric"

stationUrl :: String -> String -> String
stationUrl station appId = defUrl ++ station ++
                           "&appid=" ++ appId ++ "&units=" ++ units

getData :: String -> String -> IO (Either JSONException WeatherInfo)
getData station appId = do
    request <- parseRequest $ "GET " ++ stationUrl station appId
    res <- httpJSONEither request
    return $ getResponseBody res

formatWeather :: WeatherInfo -> Monitor String
formatWeather (WI n des t tmin tmax hu pr ws wd) =
    do t' <- showWithColors show t
       parseTemplate [n, des, t', show tmin, show tmax, show hu,
                      show pr, show ws, show wd]

runWeather :: [String] -> Monitor String
runWeather str = do
  let [station, appId] = str
  d <- io $ getData station appId
  case d of
    Left e  -> return $ show e
    Right i -> formatWeather i
