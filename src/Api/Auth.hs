{-# LANGUAGE DeriveGeneric #-}

-- These routes are mounted under /auth
module Api.Auth where

import AppM (
  AppM,
  Config (..),
 )
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import Data.Data (Proxy (..))
import Data.Password.Bcrypt (PasswordCheck (..), PasswordHash (..), checkPassword, hashPassword, mkPassword)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Model.Model (
  RegisteredUser (..),
  Unique (UniqueUsername),
 )
import Model.RunDb (runDb)
import Servant (
  Header,
  Headers,
  JSON,
  NoContent (NoContent),
  ReqBody,
  ServerError (errBody),
  StdMethod (..),
  err401,
  err403,
  err500,
  throwError,
  (:>),
 )
import Servant.API (Verb)
import Servant.API.Generic (
  Generic,
  ToServantApi,
  genericApi,
  (:-),
 )
import Servant.Auth.Server (
  FromJWT,
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

type HeadersNoContent =
  Headers
    '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
    NoContent

type PostNoContent = Verb 'POST 204 '[JSON] HeadersNoContent

type Origin = Header "Origin" Text

-- This is purposely spelled incorrectly
type Referer = Header "Referer" Text

data AuthRoutes route = AuthRoutes
  { _register ::
      route
        :- Origin
        :> Referer
        :> "register"
        :> ReqBody '[JSON] AuthRequestBody
        :> PostNoContent
  , _login ::
      route
        :- Origin
        :> Referer
        :> "login"
        :> ReqBody '[JSON] AuthRequestBody
        :> PostNoContent
  }
  deriving (Generic)

api :: Proxy (ToServantApi AuthRoutes)
api = genericApi (Proxy :: Proxy AuthRoutes)

server :: AuthRoutes (AsServerT AppM)
server =
  AuthRoutes
    { _register = registerUser
    , _login = loginUser
    }

newtype AuthenticatedUser = AuthenticatedUser
  { authenticatedUsername :: Text
  }
  deriving (Generic)

instance ToJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJSON AuthenticatedUser

instance FromJWT AuthenticatedUser

applyCookies :: AuthenticatedUser -> AppM HeadersNoContent
applyCookies authenticatedUsername = do
  Config {..} <- ask
  mCookies <-
    liftIO $
      acceptLogin configCookieSettings configJwtSettings authenticatedUsername
  case mCookies of
    Nothing -> throwError err401
    Just cookies -> pure $ cookies NoContent

allowedCorsOrigins :: [Text]
allowedCorsOrigins = ["http://localhost:8080"]

registerUser ::
  Maybe Text ->
  Maybe Text ->
  AuthRequestBody ->
  AppM HeadersNoContent
registerUser mOrigin mReferer AuthRequestBody {..} = do
  let isValidOrigin = (`elem` allowedCorsOrigins)
  case (isValidOrigin <$> mOrigin, isValidOrigin <$> mReferer) of
    (Just True, _) -> pure ()
    -- Fall through to Referer, if Origin is NOT set
    (Nothing, Just True) -> pure ()
    _ -> throwError $ err500 {errBody = "registerUser: unable to handle your request"}
  PasswordHash hashedPassword <- hashPassword $ mkPassword password
  eRegisteredUser <-
    runDb $
      insertUnique (RegisteredUser username hashedPassword)
  case eRegisteredUser of
    Left _ ->
      throwError $
        err500 {errBody = "registerUser: unable to handle your request"}
    Right Nothing ->
      throwError $ err403 {errBody = "registerUser: this username is taken"}
    Right (Just _) -> applyCookies $ AuthenticatedUser username

-- TODO: Consider locking usernames which make repeated login requests
loginUser :: Maybe Text -> Maybe Text -> AuthRequestBody -> AppM HeadersNoContent
loginUser _ _ AuthRequestBody {..} = do
  eRegisteredUser <- runDb $ getBy (UniqueUsername username)
  case eRegisteredUser of
    Left _ ->
      throwError $
        err500 {errBody = "loginUser: unable to handle your request"}
    Right Nothing ->
      throwError $ err403 {errBody = "loginUser: this username doesn't exist"}
    Right (Just (Entity _ RegisteredUser {..})) -> do
      case checkPassword (mkPassword password) (PasswordHash registeredUserHashedPassword) of
        PasswordCheckSuccess -> applyCookies (AuthenticatedUser registeredUserUsername)
        PasswordCheckFail -> throwError err401 {errBody = "loginUser: unable to log you in"}
