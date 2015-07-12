module Dickmine
    ( parseLogEntry
    , parseLogEntries
    ) where

import           Dickmine.IO
import           Dickmine.Parse (parseLogEntries, parseLogEntry)
import           Dickmine.Types
import           Pipes
import           System.IO

parseFiles :: [String] -> IO (Producer Pagehit IO ())
parseFiles fps = do
  hlogFiles <- mapM (`openFile` ReadMode) fps
  return $ concatLogFiles hlogFiles >-> splitIntoEntries >-> parseLogEntries
