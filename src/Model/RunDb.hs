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

-- TODO: In 'Api.Auth.loginUser' we use 'runDb @SqliteException' however I
-- don't know if this actually works... Figure out how to test that we catch
-- the exception
runDb :: Exception e => SqlPersistT IO a -> AppM (Either e a)
runDb query = do
    connFromPool <- asks configPool
    liftIO . try $ runSqlPool query connFromPool
