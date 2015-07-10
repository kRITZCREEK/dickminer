{-# LANGUAGE PatternGuards #-}
module Dickmine.Parse where

import           Control.Applicative
import           Data.Char
import           Data.List
import           Data.Maybe          (isJust)
import           Data.Time
import           Dickmine.Types
import           Pipes
import qualified Pipes.Prelude       as P
import           System.Locale       (defaultTimeLocale)
import           Text.Read

parseLogEntry :: [String] -> Maybe Pagehit
parseLogEntry [rrType, date, url, country, city, lat, long, ip] =
  Pagehit               <$>
   parseIP ip           <*>
   parseURL url         <*>
   parseCity city       <*>
   parseCountry country <*>
   parseLatitude lat    <*>
   parseLongitude long  <*>
   parseDateString date <*>
   parseRrType rrType
parseLogEntry _ = Nothing

parseOldLogEntry :: [String] -> Maybe Pagehit
parseOldLogEntry [dateIpRoute, country, city, lat, long, ip] =
  Pagehit                <$>
    parseIP ip           <*>
    Just url             <*>
    parseCity city       <*>
    parseCountry country <*>
    parseLatitude lat    <*>
    parseLongitude long  <*>
    parseDateString ("Date: " ++ dateString) <*>
    Just ""
  where
    (dateString, url, _) = splitDateIPRoute dateIpRoute
parseOldLogEntry _ = Nothing

parseLogEntry' x = case parseOldLogEntry x of
  Just l -> Just l
  Nothing -> parseLogEntry x

parseLogEntries :: (Monad m) => Pipe [String] Pagehit m ()
parseLogEntries = P.map parseLogEntry >-> P.filter isJust >-> P.map (\(Just x) -> x)

parseOldLogEntries :: (Monad m) => Pipe [String] Pagehit m ()
parseOldLogEntries = P.map parseOldLogEntry >-> P.filter isJust >-> P.map (\(Just x) -> x)

parseLogEntries' :: (Monad m) => Pipe [String] Pagehit m ()
parseLogEntries' = P.map parseLogEntry' >-> P.filter isJust >-> P.map (\(Just x) -> x)

splitDateIPRoute :: String -> (String, String, String)
splitDateIPRoute s = (dateString, ipString, route)
  where splitted = words s
        dateString = unwords (take 3 splitted)
        (ipString, route) =
          if length splitted <= 4
          then ("", "")
          else (splitted !! 3, splitted !! 4)

parseRrType :: String -> Maybe String
parseRrType = parseWithPrefix "Rick Roll Type: "

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
