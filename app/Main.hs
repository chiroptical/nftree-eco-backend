{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import Data.Proxy (Proxy (..))
import Network.Wai.Handler.Warp (run)
import Servant.Auth.Server
import Servant.Server

main :: IO ()
main = do
    jwtKey <- generateKey
    let jwtCfg = defaultJWTSettings jwtKey
        cookieCfg = defaultCookieSettings :. jwtCfg :. EmptyContext
        api = Proxy :: Proxy (Api '[JWT])
    run 7249 $ serveWithContext api cookieCfg (server defaultCookieSettings jwtCfg)
