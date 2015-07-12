{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List                            (group, nub, sort)
import           Data.Monoid                          (mconcat)
import           Data.Time
import           Dickmine.IO
import qualified Dickmine.Parse                       as DP
import           Dickmine.Query
import           Dickmine.Serialize
import           Dickmine.Types
import           Network.Wai.Middleware.RequestLogger
import           Pipes
import qualified Pipes.Prelude                        as P
import           System.Environment
import           System.IO
import           Web.Scotty

-- entries = do
--   hFiles <- mapM (`openFile` ReadMode) ["../data/traffic.log"]
--   P.toListM $ concatLogFiles hFiles >->
--     splitIntoEntries >-> DP.parseLogEntries'

main :: IO ()
main = do
  paths <- getArgs
  hlogFiles <- mapM (`openFile` ReadMode) paths
  entries <- P.toListM $ concatLogFiles hlogFiles >->
             splitIntoEntries >-> DP.parseLogEntries'
  let transformEntries p = groupCount $ P.toList $ each entries >-> p
  scotty 3000 $ do
    middleware logStdoutDev
    get "/all" $
      json $ P.toList $ each entries-- >-> serializePagehits
    get "/city" $
      json $ transformEntries pluckCityCoord
    get "/country" $
      json $ transformEntries pluckCountry
    get "/url" $
      json $ transformEntries pluckPage
    get "/world" $ do
      addHeader "Access-Control-Allow-Origin" "*"
      json $ P.toList $ each entries >-> pluckWorldPoints
    get "/:word" $ do
      beam <- param "word"
      redirect "http://api.asciidick.com/api/docs"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
