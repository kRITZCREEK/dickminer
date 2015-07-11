{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List          (nub)
import           Data.Monoid        (mconcat)
import           Data.Time
import           Dickmine.IO
import qualified Dickmine.Parse     as DP
import           Dickmine.Query
import           Dickmine.Types
import           Pipes
import qualified Pipes.Prelude      as P
import           System.Environment
import           System.IO
import           Web.Scotty

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

entries = do
  hFiles <- mapM (`openFile` ReadMode) ["../data/traffic.log"]
  P.toListM $ concatLogFiles hFiles >->
    splitIntoEntries >-> DP.parseLogEntries'


main = do
  args <- getArgs
  hlogFiles <- mapM (`openFile` ReadMode) args
  entries <- P.toListM $ concatLogFiles hlogFiles >->
             splitIntoEntries >-> DP.parseLogEntries'
  let transformEntries p = nub $ P.toList $ each entries >-> p
  scotty 3000 $ do
    get "/city" $
      json $ transformEntries pluckCity
    get "/country" $
      json $ transformEntries pluckCountry
    get "/url" $
      json $ transformEntries pluckPage
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
