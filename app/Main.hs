module Main where

import           Data.Aeson
import           Data.List          (intercalate, nub, replicate)
import           Data.Maybe         (mapMaybe)
import           Dickmine
import           Dickmine.Types
import           System.Environment
import           System.IO

separator :: IO ()
separator = putStrLn (replicate 25 '=')

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
  let entries = map parseLogEntry $ concatMap splitIntoEntries logFiles
  putStrLn "Raw Data:"
  putStrLn $ show entries
  separator
  putStrLn $ "There were: " ++ show (length entries) ++ " victims."
  separator
  putStrLn $ "We hit these cities: " ++ show (nub $ mapMaybe (fmap city) entries) ++ "."
