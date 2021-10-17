module Api where

import Api.Auth (AuthenticatedUser (..))
import qualified Api.Auth as Auth
import AppM (AppM)
import Data.Text (Text)
import Servant
import Servant.API.Generic (ToServantApi)
import Servant.Auth.Server (
  Auth,
  AuthResult (Authenticated),
  throwAll,
 )
import Servant.Server.Generic (genericServerT)

-- TODO: This is just a placeholder for testing...
type Protected = "protected" :> Get '[JSON] Text

protected ::
  Servant.Auth.Server.AuthResult AuthenticatedUser -> ServerT Protected AppM
protected (Servant.Auth.Server.Authenticated AuthenticatedUser {..}) =
  return authenticatedUsername
protected _ = throwAll err401

type Api auths =
  (Auth auths AuthenticatedUser :> Protected) :<|> "auth" :> ToServantApi Auth.AuthRoutes

server :: ServerT (Api auths) AppM
server = protected :<|> genericServerT Auth.server
