module Main where

import           Data.List                        (nub)
import           Data.Maybe                       (mapMaybe)
import           Dickmine
import           Dickmine.IO
import           Dickmine.Types
import           Pipes
import qualified Pipes.Prelude                    as P
import           System.Environment
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  hlogFiles <- mapM (`openFile` ReadMode) args
  let producers = map readLogFile' hlogFiles
  entries <- P.toListM $ head producers >-> splitIntoEntries
  let pageHits = map parseLogEntry entries
  putStrLn "Raw Data:"
  print pageHits
  separator
  putStrLn $ "There were: " ++ show (length pageHits) ++ " victims."
  separator
  putStrLn $ "We hit these cities: " ++ show (nub $ mapMaybe (fmap city) pageHits) ++ "."
