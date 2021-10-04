{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Database where

import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH

-- TODO:
-- - Add hashed password to RegisteredUser

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
RegisteredUser
    username Text
    UniqueUsername username
    deriving Show
|]
