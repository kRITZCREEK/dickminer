{-# LANGUAGE PatternGuards #-}
module Dickmine.Parse where

import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.Time
import           Dickmine.Types
import           System.Locale       (defaultTimeLocale)
import           Text.Read


parseLogEntry :: [String] -> Maybe Pagehit
parseLogEntry [rrType, date, url, country, city, lat, long, ip] =
  Pagehit <$>
   ip' <*>
   url' <*>
   city' <*>
   country' <*>
   lat' <*>
   long' <*>
   date'
  where
    ip' = parseIP ip
    url' = parseURL url
    country' = parseCountry country
    city' = parseCity city
    lat' = parseLatitude lat
    long' = parseLongitude long
    date' = parseDateString date
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
parseDateString ds = do
  dateString <- parseWithPrefix "Date: " ds
  parseTime defaultTimeLocale "%m/%d/%Y %I:%M:%S %p" dateString

parseIP :: String -> Maybe String
parseIP s = do
  ipString <- parseWithPrefix "IP: " s
  if all (\c -> isDigit c || c == '.') ipString
    then Just ipString
    else Nothing

parseURL :: String -> Maybe String
parseURL = parseWithPrefix "URL: "

parseCountry :: String -> Maybe String
parseCountry = parseWithPrefix "Country: "

parseCity :: String -> Maybe String
parseCity = parseWithPrefix "City: "

parseLatitude :: String -> Maybe Double
parseLatitude s = readMaybe =<< parseWithPrefix "Latitude: " s

parseLongitude :: String -> Maybe Double
parseLongitude s = readMaybe =<< parseWithPrefix "Longitude: " s

parseWithPrefix :: String -> String -> Maybe String
parseWithPrefix prefix s | Just parse <- stripPrefix prefix s = Just parse
                         | otherwise = Nothing
