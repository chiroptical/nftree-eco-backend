{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Api.Auth (AuthenticatedUser (..))
import qualified Api.Auth as Auth
import AppM (AppM)
import Data.Text (Text)
import Servant
import Servant.API.Generic (ToServantApi)
import Servant.Auth.Server (Auth, AuthResult (Authenticated), CookieSettings, JWTSettings, throwAll)
import Servant.Server.Generic (genericServerT)

type Protected = "name" :> Get '[JSON] Text

protected :: Servant.Auth.Server.AuthResult AuthenticatedUser -> ServerT Protected AppM
protected (Servant.Auth.Server.Authenticated AuthenticatedUser{..}) = return authenticatedUsername
protected _ = throwAll err401

type Api auths = (Auth auths AuthenticatedUser :> Protected) :<|> "auth" :> ToServantApi Auth.AuthRoutes

server :: CookieSettings -> JWTSettings -> ServerT (Api auths) AppM
server cs js = protected :<|> genericServerT (Auth.server cs js)
