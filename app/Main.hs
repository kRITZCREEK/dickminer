module Main where

import           Dickmine
import           System.Environment
import           System.IO

readLogFile :: FilePath  -> IO [String]
readLogFile path = fmap lines (readFile path)

splitIntoEntries :: [String] -> [[String]]
splitIntoEntries file =
  reverse $ splitIntoEntries' [] $ filter (/= "") file
  where splitIntoEntries' acc ls
          | length ls >= 6 = splitIntoEntries' (take 6 ls : acc) (drop 6 ls)
          | otherwise      = acc

main :: IO ()
main = do
  args <- getArgs
  logFiles <- mapM readLogFile args
  let entries = concatMap splitIntoEntries logFiles
  print (map parseLogEntry entries)
