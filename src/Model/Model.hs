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
RegisteredUser sql=registered_users
  email Text
  hashedPassword Text
  UniqueEmail email
  deriving Show

NFTree sql=nftrees
  commonName Text
  latinName Text
  secret Text
  UniqueSecret secret
  deriving Show
|]
