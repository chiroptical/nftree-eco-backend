module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Esqueleto.Experimental
import Database.Persist.Sqlite (
  createSqlitePool,
 )
import Model.Model
import Model.Seed (seedDatabase)
import UnliftIO (liftIO)

main :: IO ()
main = do
  configPool <- runStderrLoggingT $ createSqlitePool "nftree.prod.db" 1

  -- TODO: We may want a better error message on failures here
  liftIO . flip runSqlPool configPool $ runMigration migrateAll

  liftIO . flip runSqlPool configPool $ seedDatabase
