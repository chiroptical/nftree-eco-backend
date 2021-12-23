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
import Data.Time (UTCTime)
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
  -- Unique id
  treeId Int
  UniqueTreeId treeId

  -- Scientific names
  genus Text
  species Text
  cultivar Text Maybe
  commonName Text

  -- Facts
  postalCode Int Maybe
  diameter Double Maybe
  height Double Maybe
  datePlanted UTCTime Maybe
  dateRemoved UTCTime Maybe

  -- Garbage generated at time of insertion, can freely change
  secret Text
  UniqueSecret secret

  -- Used to determine if something changes
  updatedAt UTCTime

  deriving Show
|]
