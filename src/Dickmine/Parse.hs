{-# LANGUAGE PatternGuards #-}
module Dickmine.Parse where

import Dickmine.Types
import Data.Time
import Data.List
import Data.Char
import Text.Read
import Control.Applicative
import System.Locale (defaultTimeLocale)


parseLogEntry :: [String] -> Maybe Pagehit
parseLogEntry [dateIPRoute, country, city, lat, long, ip] =
  Pagehit <$> Just ip' <*> Just page' <*> lat' <*> long' <*> time'
  where
    (date, ip', page') = splitDateIPRoute dateIPRoute
    country' = parseCountry country
    city' = parseCity city
    lat' = parseLatitude lat
    long' = parseLongitude long
    time' = parseDateString date
parseLogEntry _ = Nothing

splitDateIPRoute :: String -> (String, String, String)
splitDateIPRoute s = (dateString, ipString, route)
  where splitted = words s
        dateString = unwords (take 3 splitted)
        (ipString, route) =
          if length splitted <= 4
          then ("", "")
          else (splitted !! 3, splitted !! 4)


parseDateString :: String -> Maybe UTCTime
parseDateString = parseTime defaultTimeLocale "%m/%d/%Y %I:%M:%S %p"

parseIP :: String -> Maybe String
parseIP s =
  if all (\c -> isDigit c || c == '.') s
  then Just s
  else Nothing

parseCountry :: String -> Maybe String
parseCountry = parseWithPrefix "Country: "

parseCity :: String -> Maybe String
parseCity = parseWithPrefix "City: "

parseLatitude :: String -> Maybe Double
parseLatitude "Latitude:" = Just 0
parseLatitude s = readMaybe =<< parseWithPrefix "Latitude: " s

parseLongitude :: String -> Maybe Double
parseLongitude "Longitude:" = Just 0
parseLongitude s = readMaybe =<< parseWithPrefix "Longitude: " s

parseWithPrefix :: String -> String -> Maybe String
parseWithPrefix prefix s | Just parse <- stripPrefix prefix s = Just parse
                         | otherwise = Nothing
