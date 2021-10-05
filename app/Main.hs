{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import Network.Wai.Handler.Warp (run)
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
    let config = Config{..}

    -- TODO: We may want a better error message on failures here
    liftIO . flip runSqlPool configPool $ runMigration migrateAll

    run 8081
        . serveWithContext api cookieContext
        . hoistServerWithContext api contextProxy (`runReaderT` config)
        $ server
