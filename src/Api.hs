{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Api.Auth (AuthenticatedUser (..))
import qualified Api.Auth as Auth
import Data.Text (Text)
import Servant
import Servant.API.Generic (ToServantApi)
import Servant.Auth.Server (Auth, AuthResult (Authenticated), CookieSettings, JWTSettings, throwAll)
import Servant.Server.Generic (genericServer)

type Protected = "name" :> Get '[JSON] Text

protected :: Servant.Auth.Server.AuthResult AuthenticatedUser -> Server Protected
protected (Servant.Auth.Server.Authenticated AuthenticatedUser{..}) = return authenticatedUsername
protected _ = throwAll err401

type Api auths = (Auth auths AuthenticatedUser :> Protected) :<|> "auth" :> ToServantApi Auth.AuthRoutes

server :: CookieSettings -> JWTSettings -> Server (Api auths)
server cs js = protected :<|> genericServer (Auth.server cs js)
