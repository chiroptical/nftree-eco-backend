{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import qualified Api.Auth as Auth
import Servant ((:>))
import Servant.API.Generic (ToServantApi)

type Api = "auth" :> ToServantApi Auth.AuthRoutes
