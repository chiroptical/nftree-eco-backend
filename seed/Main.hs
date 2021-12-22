module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Esqueleto.Experimental
import Database.Persist.Sqlite (
  createSqlitePool,
 )
import Model.Model
import Model.Seed (seedDatabase, unsafeDropRegisteredUsers)
import UnliftIO (liftIO)

main :: IO ()
main = do
  configPool <- runStderrLoggingT $ createSqlitePool "nftree.prod.db" 1
  liftIO . flip runSqlPool configPool $ do
    _ <- unsafeDropRegisteredUsers
    runMigration migrateAll
    seedDatabase
