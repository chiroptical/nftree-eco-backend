module AppM where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import Servant (Handler)

newtype Config = Config
    { configPool :: Pool SqlBackend
    }

type AppM = ReaderT Config Handler
