{-# LANGUAGE DeriveGeneric #-}

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

decodeTreeCsv :: FilePath -> IO (Either String (Vector TreeRow))
decodeTreeCsv fp = do
  contents <- readFile fp
  pure $ decode HasHeader (fromString contents)

data TreeRow = TreeRow
  { latitude :: Text
  , longitude :: Text
  , streetAddress :: Text
  , city :: Text
  , -- TODO: Maybe 'xxxxx-xxx'?
    postalCode :: Text
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
  , treeHeight :: Maybe Text
  , canopyHeight :: Text
  , datePlanted :: Maybe Time
  , -- nf trees are non-refungible
    dateRemoved :: Time
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

data TreePresence
  = IsPresent
  | IsNotPresent
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
