{-# LANGUAGE DeriveGeneric #-}

module Dickmine.Types where

import Data.Time (UTCTime)
import Data.Aeson
import GHC.Generics

data Pagehit =
  Pagehit {
    ip        :: String,
    page      :: String,
    latitude  :: Double,
    longitude :: Double,
    timestamp :: UTCTime
    } deriving(Show, Eq, Generic)

instance ToJSON Pagehit
