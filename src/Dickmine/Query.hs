module Dickmine.Query where

import           Data.Time
import           Dickmine.Types
import           Pipes
import qualified Pipes.Prelude  as P

pluckCity :: (Monad m) => Pipe Pagehit String m ()
pluckCity = P.map city

pluckCountry :: (Monad m) => Pipe Pagehit String m ()
pluckCountry = P.map country

pluckPage :: (Monad m) => Pipe Pagehit String m ()
pluckPage = P.map page

pageHitsBetween :: (Monad m) => UTCTime -> UTCTime -> Pipe Pagehit Pagehit m ()
pageHitsBetween from till =
  P.filter pageHitBetween
  where
    pageHitBetween ph = let ts = timestamp ph
                        in from <= ts && ts >= till
