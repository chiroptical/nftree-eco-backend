{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- These routes are mounted under /auth
module Api.Auth where

import           AppM
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , asks
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Data                      ( Proxy(..) )
import           Data.Password.Bcrypt
import           Data.Text                      ( Text )
import           Database.Esqueleto.Experimental
import           Servant                        ( (:>)
                                                , Header
                                                , Headers
                                                , JSON
                                                , NoContent(NoContent)
                                                , ReqBody
                                                , ServerError(errBody)
                                                , StdMethod(POST)
                                                , err401
                                                , err403
                                                , throwError
                                                )
import           Servant.API                    ( Verb )
import           Servant.API.Generic            ( (:-)
                                                , Generic
                                                , ToServantApi
                                                , genericApi
                                                )
import           Servant.Auth.Server            ( FromJWT
                                                , SetCookie
                                                , ToJWT
                                                , acceptLogin
                                                )
import           Servant.Server.Generic         ( AsServerT )
import           Type.Database                  ( RegisteredUser(..)
                                                , Unique(UniqueUsername)
                                                )
import           UnliftIO                       ( liftIO )

data AuthRequestBody = AuthRequestBody
    { username :: Text
    , password :: Text
    }
    deriving Generic

instance ToJSON AuthRequestBody
instance ToJWT AuthRequestBody
instance FromJSON AuthRequestBody
instance FromJWT AuthRequestBody

type HeadersNoContent
    = Headers
          '[Header "Set-Cookie" SetCookie , Header "Set-Cookie" SetCookie]
          NoContent
type PostNoContent = Verb 'POST 204 '[JSON] HeadersNoContent

data AuthRoutes route
    = AuthRoutes
        { _register
              :: route :- "register" :> ReqBody '[JSON] AuthRequestBody :> PostNoContent
        , _login
              :: route :- "login" :> ReqBody '[JSON] AuthRequestBody :> PostNoContent
        }
    deriving Generic

api :: Proxy (ToServantApi AuthRoutes)
api = genericApi (Proxy :: Proxy AuthRoutes)

server :: AuthRoutes (AsServerT AppM)
server = AuthRoutes { _register = registerUser, _login = loginUser }

newtype AuthenticatedUser = AuthenticatedUser
    { authenticatedUsername :: Text
    }
    deriving (Generic)

instance ToJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJSON AuthenticatedUser
instance FromJWT AuthenticatedUser

-- TODO: How to deal with errors here?
runDb :: SqlPersistT IO a -> AppM a
runDb query = do
    connFromPool <- asks configPool
    liftIO $ runSqlPool query connFromPool

applyCookies :: AuthenticatedUser -> AppM HeadersNoContent
applyCookies authenticatedUsername = do
    Config {..} <- ask
    mCookies    <- liftIO $ acceptLogin configCookieSettings
                                        configJwtSettings
                                        authenticatedUsername
    case mCookies of
        Nothing      -> throwError err401
        Just cookies -> pure $ cookies NoContent

registerUser :: AuthRequestBody -> AppM HeadersNoContent
registerUser AuthRequestBody {..} = do
    PasswordHash hashedPassword <- hashPassword $ mkPassword password
    mRegisteredUser             <- runDb
        $ insertUnique (RegisteredUser username hashedPassword)
    case mRegisteredUser of
        Nothing -> throwError $ err403 { errBody = "The username is taken" }
        Just _  -> applyCookies $ AuthenticatedUser username

loginUser :: AuthRequestBody -> AppM HeadersNoContent
loginUser AuthRequestBody {..} = do
    mRegisteredUser <- runDb $ getBy (UniqueUsername username)
    case mRegisteredUser of
        Nothing -> throwError $ err403 { errBody = "Username doesn't exist" }
        Just (Entity _ RegisteredUser {..}) -> do
            PasswordHash passwordHash <- hashPassword $ mkPassword password
            if passwordHash == registeredUserHashedPassword
                then applyCookies (AuthenticatedUser registeredUserUsername)
                else throwError err401
