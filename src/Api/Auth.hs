{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- These routes are mounted under /auth
module Api.Auth where

import AppM
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
import Servant.Server.Generic (AsServerT)
import UnliftIO (liftIO)

data AuthRequestBody = AuthRequestBody
    { username :: Text
    , password :: Text
    }
    deriving (Generic)

instance ToJSON AuthRequestBody
instance ToJWT AuthRequestBody
instance FromJSON AuthRequestBody
instance FromJWT AuthRequestBody

type HeadersNoContent = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent
type PostNoContent = Verb 'POST 204 '[JSON] HeadersNoContent

newtype AuthRoutes route = AuthRoutes
    { -- _register :: route :- ReqBody '[JSON] AuthRequestBody :> PostNoContent,
      _login :: route :- "login" :> ReqBody '[JSON] AuthRequestBody :> PostNoContent
    }
    deriving (Generic)

api :: Proxy (ToServantApi AuthRoutes)
api = genericApi (Proxy :: Proxy AuthRoutes)

server :: CookieSettings -> JWTSettings -> AuthRoutes (AsServerT AppM)
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
    AppM HeadersNoContent
checkCreds cookieSettings jwtSettings AuthRequestBody{..} = do
    mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings (AuthenticatedUser username)
    case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> pure $ applyCookies NoContent
