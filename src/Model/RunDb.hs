module Model.RunDb (runDb) where

import AppM
  ( AppM,
    Config (..),
  )
import Control.Monad.Reader (asks)
import Database.Esqueleto.Experimental
  ( SqlPersistT,
  )
import Database.Persist.Sql (runSqlPool)
import Database.Sqlite (SqliteException (..))
import UnliftIO
  ( Exception,
    MonadIO (liftIO),
    try,
  )

-- | 'runSqlPool' should only raise 'SqliteException's and therefore we could
-- really just use this specialized to 'SqliteException'. However, I am leaving
-- this here for posterity. It is important to understand that we can't do
-- something like this for a function that can throw arbitrary exceptions.
-- Additionally, this may not handle asynchronous exceptions but I don't believe
-- 'runSqlPool' should throw any async exceptions.
runDb' :: Exception e => SqlPersistT IO a -> AppM (Either e a)
runDb' query = do
  connFromPool <- asks configPool
  liftIO . try $ runSqlPool query connFromPool

runDb :: SqlPersistT IO a -> AppM (Either SqliteException a)
runDb = runDb' @SqliteException
