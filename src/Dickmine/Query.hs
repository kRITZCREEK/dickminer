{-# LANGUAGE DeriveGeneric #-}
module Dickmine.Query where

import           Control.Arrow  ((&&&))
import           Data.Aeson
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

pluckCity :: (Monad m) => Pipe Pagehit String m ()
pluckCity = P.map city

pluckCityCoord :: (Monad m) => Pipe Pagehit CityResponse m ()
pluckCityCoord = P.map (\ph -> CityResponse (city ph) (latitude ph) (longitude ph))

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
