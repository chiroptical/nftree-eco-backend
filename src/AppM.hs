module AppM where

import Control.Monad.Trans.Reader (ReaderT)
import Database.Esqueleto.Experimental (
    ConnectionPool,
 )
import Servant (Handler)
import Servant.Auth.Server (
    CookieSettings,
    JWTSettings,
 )

data Config = Config
    { configPool :: ConnectionPool
    , configCookieSettings :: CookieSettings
    , configJwtSettings :: JWTSettings
    }

type AppM = ReaderT Config Handler
