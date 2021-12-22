module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Csv (HasHeader (HasHeader), decode)
import Data.TreeCsv (TreeRow)
import Data.Vector (Vector)
import Database.Esqueleto.Experimental
import Database.Persist.Sqlite (
  createSqlitePool,
 )
import Model.Model
import Model.Seed (seedDatabase, unsafeDropRegisteredUsers)
import UnliftIO (liftIO)

main :: IO ()
main = do
  contents <- readFile "./tree-data-example.csv"
  let results = decode HasHeader (fromString contents) :: Either String (Vector TreeRow)
  print results

-- configPool <- runStderrLoggingT $ createSqlitePool "nftree.prod.db" 1
-- liftIO . flip runSqlPool configPool $ do
--   _ <- unsafeDropRegisteredUsers
--   runMigration migrateAll
--   seedDatabase
