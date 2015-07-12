{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Dickmine                             (parseFiles)
import           Dickmine.Query
import           Network.Wai.Middleware.RequestLogger
import           Pipes
import qualified Pipes.Prelude                        as P
import           System.Environment
import           Web.Scotty

main :: IO ()
main = do
  entries <- P.toListM =<< parseFiles =<< getArgs
  let transformEntries p = groupCount $ P.toList $ each entries >-> p
  scotty 3000 $ do
    middleware logStdoutDev
    get "/all" $
      json entries
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
      redirect "http://api.asciidick.com/api/docs"
