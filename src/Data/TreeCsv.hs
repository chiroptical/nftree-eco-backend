{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.TreeCsv where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Csv (FromField (..), FromRecord, HasHeader (HasHeader), ToField (..), ToRecord, decode)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 qualified as Iso
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()
import Text.Read (readMaybe)

decodeTreeCsv :: FilePath -> IO (Either String (Vector TreeRow))
decodeTreeCsv fp = do
  contents <- readFile fp
  pure $ decode HasHeader (fromString contents)

data TreeRow = TreeRow
  { latitude :: Text
  , longitude :: Text
  , streetAddress :: Text
  , city :: Text
  , postalCode :: Maybe PostalCode
  , plantingSiteWidth :: Text
  , plantingSiteLength :: Text
  , plantingSiteId :: Text
  , customId :: Text
  , updatedAt :: Time
  , updatedBy :: Text
  , treeId :: Maybe Int
  , treePresent :: TreePresence
  , genus :: Text
  , species :: Text
  , -- a plant variety that has been produced in cultivation by selective breeding
    cultivar :: Text
  , otherPartOfName :: Text
  , commonName :: Text
  , -- This may not be present, in cassava you can just use 'Maybe a'
    -- to represent it
    diameter :: Maybe Double
  , treeHeight :: Maybe Double
  , canopyHeight :: Text
  , datePlanted :: Maybe Time
  , -- nf trees are non-refungible
    dateRemoved :: Maybe Time
  , amountOfPruning :: Text
  , confidenceInIdentification :: Text
  , isMultistem :: Text
  , mcbTreeNumber :: Text
  , melneaCassTrees :: Text
  , notes :: Text
  , otherFeatures :: Text
  , percentGreen :: Text
  , stewardship :: Text
  , condition :: Text
  , treeIs :: Text
  , fenceOrPerimeter :: Text
  , metalGratePresent :: Text
  , looseTrashPresent :: Text
  , raisedBed :: Text
  , isStump :: Text
  , materialInSite :: Text
  , pitLength :: Text
  , plantingSiteNotes :: Text
  , plantingSiteType :: Text
  , plantingSiteWidth_ :: Text
  , qaCheck :: Text
  , qaDeletion :: Text
  , qaNotes :: Text
  , concreteCutouts :: Text
  , siteSidewalkWidth :: Text
  , sidewalkWidth :: Text
  , plantingSiteStewardship :: Text
  , dedicatedTo :: Text
  , wiresOverhead :: Text
  }
  deriving (Generic, Show)

instance FromRecord TreeRow

instance ToRecord TreeRow

newtype Time = Time UTCTime
  deriving (Eq, Show)

instance ToField Time where
  toField (Time utcTime) = toField $ Iso.iso8601Show utcTime

instance FromField Time where
  parseField bs = do
    let t :: Text = decodeUtf8 bs
        (front, _) = Text.breakOn "+" t
        len = Text.length "2019-12-21T17:55:21.687"
        adjustFront = Text.take len front
    parsedTime <- Iso.iso8601ParseM (Text.unpack $ adjustFront <> "Z")
    pure $ Time parsedTime

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary

data TreePresence
  = IsPresent
  | IsNotPresent
  deriving (Eq, Show)

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

instance Arbitrary TreePresence where
  arbitrary = frequency [(9, pure IsPresent), (1, pure IsNotPresent)]

newtype PostalCode = PostalCode Int
  deriving newtype (Eq, Show, ToField, Arbitrary)

instance FromField PostalCode where
  parseField bs = do
    let t :: Text = decodeUtf8 bs
        (first, _) = Text.breakOn "-" t
        mPostalCode = readMaybe @Int (Text.unpack first)
    case mPostalCode of
      Just postalCode -> pure $ PostalCode postalCode
      Nothing -> fail $ "Unable to decode as postal code, got: " <> Text.unpack t
