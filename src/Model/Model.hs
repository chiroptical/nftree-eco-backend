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

module Model.Model where

import Data.Text (Text)
import Database.Persist.TH (
    mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
 )

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
RegisteredUser
    username Text
    hashedPassword Text
    UniqueUsername username
    deriving Show
|]
