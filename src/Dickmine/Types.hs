{-# LANGUAGE DeriveGeneric #-}

module Dickmine.Types where

import           Data.Aeson
import           Data.Time    (UTCTime)
import           GHC.Generics

data Pagehit =
  Pagehit {
    ip        :: String,
    page      :: String,
    city      :: String,
    country   :: String,
    latitude  :: Double,
    longitude :: Double,
    timestamp :: UTCTime
    } deriving(Show, Eq, Generic)

instance ToJSON Pagehit
