{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- These routes are mounted under /auth
module Api.Auth where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Proxy (..))
import Data.Text (Text)
import Servant (Header, Headers, JSON, NoContent (NoContent), ReqBody, StdMethod (POST), err401, throwError, (:>))
import Servant.API (Verb)
import Servant.API.Generic (
    Generic,
    ToServantApi,
    genericApi,
    (:-),
 )
import Servant.Auth.Server (
    CookieSettings,
    FromJWT,
    JWTSettings,
    SetCookie,
    ToJWT,
    acceptLogin,
 )
import Servant.Server (Handler)
import Servant.Server.Generic (AsServer)

data AuthRequestBody = AuthRequestBody
    { username :: Text
    , password :: Text
    }

type HeadersNoContent = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent
type PostNoContent = Verb 'POST 204 '[JSON] HeadersNoContent

newtype AuthRoutes route = AuthRoutes
    { -- _register :: route :- ReqBody '[JSON] AuthRequestBody :> PostNoContent,
      _login :: route :- ReqBody '[JSON] AuthRequestBody :> PostNoContent
    }
    deriving (Generic)

api :: Proxy (ToServantApi AuthRoutes)
api = genericApi (Proxy :: Proxy AuthRoutes)

server :: CookieSettings -> JWTSettings -> AuthRoutes AsServer
server cs js =
    AuthRoutes
        { _login = checkCreds cs js
        }

newtype AuthenticatedUser = AuthenticatedUser
    { authenticatedUsername :: Text
    }
    deriving (Generic)

instance ToJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJSON AuthenticatedUser
instance FromJWT AuthenticatedUser

checkCreds ::
    CookieSettings ->
    JWTSettings ->
    AuthRequestBody ->
    Handler HeadersNoContent
checkCreds cookieSettings jwtSettings (AuthRequestBody "chiroptical" "chiroptical") = do
    -- TODO: Need to hash with 'password' and check against database
    mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (AuthenticatedUser "chiroptical")
    case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ _ = throwError err401
