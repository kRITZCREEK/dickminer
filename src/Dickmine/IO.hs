module Dickmine.IO where

import           Control.Monad                    (forever)
import           Control.Monad.Trans.State.Strict
import           Pipes
import qualified Pipes.Prelude                    as P
import           System.IO                        (Handle)

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
