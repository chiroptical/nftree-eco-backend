module Model.RunDb where

import AppM (
    AppM,
    Config (..),
 )
import Control.Monad.Reader (asks)
import Database.Esqueleto.Experimental (
    SqlPersistT,
 )
import Database.Persist.Sql (runSqlPool)
import UnliftIO (
    Exception,
    MonadIO (liftIO),
    try,
 )

-- TODO: It would be a ton better if we could convert this exception into a
-- custom exception tailored to our needs
runDb :: Exception e => SqlPersistT IO a -> AppM (Either e a)
runDb query = do
    connFromPool <- asks configPool
    liftIO . try $ runSqlPool query connFromPool
