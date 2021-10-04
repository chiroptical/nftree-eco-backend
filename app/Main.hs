{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Api (Api, server)
import AppM
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Proxy (Proxy (..))
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (run)
import Servant.Auth.Server
import Servant.Server
import Type.Database
import UnliftIO (liftIO)

main :: IO ()
main = do
    -- TODO: Need to store this key in the database on application start?
    jwtKey <- generateKey
    let jwtSettings = defaultJWTSettings jwtKey
        cookieSettings = defaultCookieSettings :. jwtSettings :. EmptyContext
        api = Proxy :: Proxy (Api '[JWT])
        contextProxy = Proxy :: Proxy '[CookieSettings, JWTSettings]
    runStderrLoggingT . withSqlitePool "nftree.prod.db" 1 $ \configPool -> liftIO $ do
        runResourceT . flip runSqlPool configPool $ runMigration migrateAll
        run 8081
            . serveWithContext api cookieSettings
            . hoistServerWithContext api contextProxy (`runReaderT` Config{..})
            $ server defaultCookieSettings jwtSettings
