{-# LANGUAGE DeriveGeneric #-}

module Data.TreeCsv where

import Data.Csv (FromField (..), FromRecord, ToField (..), ToRecord)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 qualified as Iso
import GHC.Generics (Generic)

data TreeRow = TreeRow
  { latitude :: !Double
  , longitude :: !Double
  , streetAddress :: !Text
  , postalCode :: !Integer
  , plantingSiteWidth :: !Integer
  , plantingSiteLength :: !Integer
  , plantingSiteId :: !Integer
  , customId :: !Integer
  , updatedAt :: !Time
  , updatedBy :: !Text
  , treeId :: !Integer
  , treePresent :: !TreePresence
  , genus :: !Text
  , species :: !Text
  , cultivar :: !Text
  , otherPartOfName :: !Text
  , commonName :: !Text
  , diameter :: !Double
  , treeHeight :: !Double
  , canopyHeight :: !Double
  , datePlanted :: !Time
  , dateRemoved :: !Time
  , amountOfPruning :: !Text
  , confidenceInIdentification :: !Text
  , isMultistem :: !Text
  , mcbTreeNumber :: !Text
  , melneaCassTrees :: !Text
  , notes :: !Text
  , otherFeatures :: !Text
  , percentGreen :: !Text
  , stewardship :: !Text
  , condition :: !Text
  , treeIs :: !Text
  , fenceOrPerimeter :: !Text
  , metalGratePresent :: !Text
  , looseTrashPresent :: !Text
  , raisedBed :: !Text
  , isStump :: !Text
  , materialInSite :: !Text
  , pitLength :: !Text
  , plantingSiteNotes :: !Text
  , qaCheck :: !Text
  , qaDeletion :: !Text
  , qaNotes :: !Text
  , concreteCutouts :: !Text
  , siteSidewalkWidth :: !Text
  , sidewalkWidth :: !Text
  , plantingSiteStewardship :: !Text
  , dedicatedTo :: !Text
  , wiresOverhead :: !Text
  }
  deriving (Generic, Show)

instance FromRecord TreeRow

instance ToRecord TreeRow

newtype Time = Time UTCTime
  deriving (Eq, Show)

data TreePresence
  = IsPresent
  | IsNotPresent
  deriving (Eq, Show)

instance ToField Time where
  toField (Time utcTime) = toField $ Iso.iso8601Show utcTime

instance FromField Time where
  parseField bs = do
    -- TODO: Consider adding https://hackage.haskell.org/package/utf8-string 'toString'?
    let s = Text.unpack . decodeUtf8 $ bs
    -- TODO: We store this as UTCTime but we don't handle the timezone...
    parsedTime <- Iso.iso8601ParseM s
    pure $ Time parsedTime

instance ToField TreePresence where
  toField = \case
    IsPresent -> "True"
    IsNotPresent -> "False"

instance FromField TreePresence where
  parseField bs =
    case bs of
      "True" -> pure IsPresent
      "False" -> pure IsNotPresent
      _ -> fail $ "Got '" <> (Text.unpack . decodeUtf8 $ bs) <> "' but expected 'True' or 'False'"
