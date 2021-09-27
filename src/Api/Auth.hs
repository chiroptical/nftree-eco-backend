{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- These routes are mounted under /auth
module Api.Auth where

import Data.Data (Proxy (..))
import Data.Text (Text)
import Servant (JSON, Post, ReqBody, (:>))
import Servant.API.Generic (
    Generic,
    ToServantApi,
    genericApi,
    (:-),
 )
import Servant.Server.Generic (AsServer)

data AuthRequestBody = AuthRequestBody
    { username :: Text
    , password :: Text
    }

newtype AuthResult = AuthResult
    { success :: Bool
    }

data Auth route = Auth
    { _register :: route :- ReqBody '[JSON] AuthRequestBody :> Post '[JSON] AuthResult
    , _login :: route :- ReqBody '[JSON] AuthRequestBody :> Post '[JSON] AuthResult
    }
    deriving (Generic)

api :: Proxy (ToServantApi Auth)
api = genericApi (Proxy :: Proxy Auth)

server :: Auth AsServer
server =
    Auth
        { _register = const . pure $ AuthResult True
        , _login = const . pure $ AuthResult True
        }
