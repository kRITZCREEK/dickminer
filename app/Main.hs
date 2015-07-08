module Main where

import           Control.Applicative
import           Control.Monad                    (forever)
import           Control.Monad.Trans.State.Strict
import           Data.List                        (nub)
import           Data.Maybe                       (mapMaybe)
import           Dickmine
import           Dickmine.Types
import           Pipes
import qualified Pipes.Prelude                    as P
import           System.Environment
import           System.IO

separator :: IO ()
separator = putStrLn (replicate 25 '=')

readLogFile' :: Handle -> Producer String IO ()
readLogFile' = P.fromHandle

splitIntoEntries :: Pipe String [String] IO ()
splitIntoEntries = evalStateT (forever splitIntoEntries') ("", [])

splitIntoEntries' :: StateT (String, [String]) (Pipe String [String] IO) ()
splitIntoEntries' = do
  nextVal <- lift await
  (lastVal, acc) <- get
  if nextVal == ""
    then if lastVal == ""
         then put ("whatever", []) >> lift (yield acc)
         else put ("", acc)
    else put (nextVal, acc ++ [nextVal])

main :: IO ()
main = do
  args <- getArgs
  hlogFiles <- mapM (`openFile` ReadMode) args
  let producers = map readLogFile' hlogFiles
  entries <- P.toListM $ head producers >-> splitIntoEntries
  let pageHits = map parseLogEntry entries
  putStrLn "Raw Data:"
  separator
  putStrLn $ "There were: " ++ show (length pageHits) ++ " victims."
  separator
  putStrLn $ "We hit these cities: " ++ show (nub $ mapMaybe (fmap city) pageHits) ++ "."
