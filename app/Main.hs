module Main where

import Api (
  Api,
  server,
 )
import AppM (Config (..))
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Proxy (Proxy (..))
import Database.Persist.Sqlite (
  createSqlitePool,
  runMigration,
  runSqlPool,
 )
import Model.Model (migrateAll)
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (..),
  cors,
  simpleCorsResourcePolicy,
 )
import Servant.Auth.Server (
  CookieSettings,
  JWT,
  JWTSettings,
  defaultCookieSettings,
  defaultJWTSettings,
  generateKey,
 )
import Servant.Server (
  Context (..),
  hoistServerWithContext,
  serveWithContext,
 )
import UnliftIO (liftIO)

main :: IO ()
main = do
  -- TODO: Need to store this key in the database on application start?
  jwtKey <- generateKey
  let configJwtSettings = defaultJWTSettings jwtKey
      configCookieSettings = defaultCookieSettings
      cookieContext = configCookieSettings :. configJwtSettings :. EmptyContext
      api = Proxy :: Proxy (Api '[JWT])
      contextProxy = Proxy :: Proxy '[CookieSettings, JWTSettings]

  configPool <- runStderrLoggingT $ createSqlitePool "nftree.prod.db" 1
  let config = Config {..}

  -- TODO: We may want a better error message on failures here
  liftIO . flip runSqlPool configPool $ runMigration migrateAll

  -- TODO: We need to include a library for configuring the application
  withStdoutLogger $ \aplogger -> do
    -- TODO: port should go into Environment
    let settings = setPort 3001 $ setLogger aplogger defaultSettings
    runSettings settings
      . cors
        ( const $
            Just
              simpleCorsResourcePolicy
                { corsOrigins = Just (["http://localhost:3000"], True)
                , corsRequestHeaders = ["Content-Type"]
                }
        )
      . serveWithContext api cookieContext
      . hoistServerWithContext api contextProxy (`runReaderT` config)
      $ server
