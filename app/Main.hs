module Main where

import           Data.List          (nub)
import           Dickmine           (parseFiles)
import           Dickmine.IO
import           Dickmine.Query
import           Pipes
import qualified Pipes.Prelude      as P
import           System.Environment

main :: IO ()
main = do
  putStrLn "What information do you want to see? (city, country, page, all)"
  query <- getLine
  let query' =
        case query of
          "city" -> pluckCity
          "country" -> pluckCountry
          "page" -> pluckPage
          "all" -> P.map show
          _ -> P.map show
  parsed <- parseFiles =<< getArgs
  entries <- P.toListM $ parsed >-> query'

  separator
  putStrLn "Your query result:"
  print entries
  separator
  putStrLn $ "There were: " ++ show (length entries) ++ " hits."
  separator
  putStrLn "These were the unique hits:"
  separator
  print (nub entries)
