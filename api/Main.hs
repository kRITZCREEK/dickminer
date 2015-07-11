{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List          (nub)
import           Data.Monoid        (mconcat)
import           Dickmine.IO
import qualified Dickmine.Parse     as DP
import           Dickmine.Query
import           Pipes
import qualified Pipes.Prelude      as P
import           System.Environment
import           System.IO
import           Web.Scotty

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
