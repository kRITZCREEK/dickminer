{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Dickmine.Query where

import           Control.Arrow  ((&&&))
import           Data.Aeson
import           Data.List      (nub)
import qualified Data.Map       as Map
import           Data.Time
import           Dickmine.Types
import           GHC.Generics
import           Pipes
import qualified Pipes.Prelude  as P


data CityResponse =
  CityResponse {
    cityName      :: String,
    cityLongitude :: Double,
    cityLatitude  :: Double
    }deriving (Show, Eq, Ord, Generic)

instance ToJSON CityResponse

mkCityResponse :: Pagehit -> CityResponse
mkCityResponse ph = CityResponse (city ph) (latitude ph) (longitude ph)

pluckCity :: (Monad m) => Pipe Pagehit String m ()
pluckCity = P.map city

pluckCityCoord :: (Monad m) => Pipe Pagehit CityResponse m ()
pluckCityCoord = P.map mkCityResponse

pluckCountry :: (Monad m) => Pipe Pagehit String m ()
pluckCountry = P.map country

pluckPage :: (Monad m) => Pipe Pagehit String m ()
pluckPage = P.map page

pluckWorldPoints :: (Monad m) => Pipe Pagehit (Double, Double) m ()
pluckWorldPoints = P.map (latitude &&& longitude)

pageHitsBetween :: (Monad m) => UTCTime -> UTCTime -> Pipe Pagehit Pagehit m ()
pageHitsBetween from till =
  P.filter pageHitBetween
  where
    pageHitBetween ph = let ts = timestamp ph
                        in from <= ts && ts >= till

groupCount :: (Ord a) => [a] -> [(a, Int)]
groupCount = Map.toList . Map.fromListWith (+) . map (, 1)

{-
data Query = Query Filter Aggregation

data Filter = City String
            | Country String
            | Url String
            | Before UTCTime
            | After UTCTime

data Aggregation = Length
                 | JSON

data QueryResult a = Raw String
                   | Encoded a
                     deriving(Show)

queryPagehits :: Query -> [Pagehit] -> QueryResult [Pagehit]
queryPagehits (Query f a) ps = aggregate a $ filter' f ps
  where
    filter' (City s) = filter ((==) s . city)
    filter' (Country s) = filter ((==) s . country)
    filter' (Url s) = filter ((==) s . page)
    filter' (Before t) = filter ((<) t . timestamp)
    filter' (After t) = filter ((>=) t . timestamp)

    aggregate Length = Raw . show . length
    aggregate JSON = Encoded . nub
-}
