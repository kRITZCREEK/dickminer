module Dickmine.Serialize where

import Dickmine.Types
import Data.Aeson
import Pipes
import Pipes.Prelude as P


serialize :: Pagehit -> String
serialize = Prelude.show . encode

serializePagehits :: (Monad m) => Pipe Pagehit String m ()
serializePagehits = P.map serialize
