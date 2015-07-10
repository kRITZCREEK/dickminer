module Main where

import           Dickmine
import           Dickmine.IO
import qualified Dickmine.Parse     as DP
import           Dickmine.Query
import           Dickmine.Types
import           Pipes
import qualified Pipes.Prelude      as P
import           System.Environment
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  putStrLn "What information do you want to see? (city, country, page, all)"
  query <- getLine
  let query' =
        case query of
          "city" -> pluckCity
          "country" -> pluckCountry
          "page" -> pluckPage
          "all" -> P.map show
  hlogFiles <- mapM (`openFile` ReadMode) args
  entries <- P.toListM $
             concatLogFiles hlogFiles >-> splitIntoEntries >-> DP.parseLogEntries >-> query'

  separator
  putStrLn "Your query result:"
  print entries
  separator
  putStrLn $ "There were: " ++ show (length entries) ++ " hits."
  separator
