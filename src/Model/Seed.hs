module Model.Seed (
  unsafeDropRegisteredUsers,
  seedDatabase,
) where

import Control.Monad (forM_, void)
import Crypto.Hash
import Data.Password.Bcrypt (PasswordHash (..), hashPassword, mkPassword)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.TreeCsv (PostalCode (..))
import Database.Esqueleto.Experimental
import Model.Model (NFTree (..), RegisteredUser (..))
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()
import UnliftIO (MonadIO, liftIO)

unsafeDropRegisteredUsers :: MonadIO m => SqlPersistT m [Single Int]
unsafeDropRegisteredUsers = rawSql "DROP TABLE IF EXISTS registered_users" []

seedDatabase :: SqlPersistT IO ()
seedDatabase = do
  PasswordHash hashedPassword <- hashPassword $ mkPassword "chiroptical"
  void $
    insertUnique
      RegisteredUser
        { registeredUserEmail = "chiroptical@email.com"
        , registeredUserHashedPassword = hashedPassword
        }
  forM_ [0 :: Int .. 100] $ \_ -> do
    nFTree <- liftIO mkTree
    void $ insertUnique nFTree

genScientificName :: Gen (Text, Text, Maybe Text, Text)
genScientificName = do
  genus <- elements ["Platanus", "Tilia", "Zelkova", "Acer", "Ulmus", "Gleditsia", "Fraxinus", "Celtis", "Pyrus", "Quercus", "Koelreuteria", "Nyssa", "Amelanchier", "Liriodendron", "Liquidambar", "Prunus", "Malus", "Ginkgo", "Gingko"]
  species <- elements ["acerifolia", "americana", "rubrum", "triacanthos", "cordata", "pennsylvanica", "occidentalis", "calleryana", "acutissima", "alba", "phellos", "paniculata", "rubra", "sylvatica", "imbricaria", "serrata", "elegans", "tomentosa", "palustris"]
  cultivar <- elements ["Greenspire", "Schwedleri", "Crimson King", "Fastigiata", "Hollandica", "Harvest Gold", "Glauca"]
  commonName <- elements ["London planetree", "American basswood", "Zelkova", "Red maple", "Elm", "Honeylocust", "Ash", "Littleleaf linden", "Green ash", "Northern hackberry", "Callery pear", "Maple", "Sawtooth oak", "White oak", "Willow oak", "Goldenrain tree", "Northern red oak", "Black tupelo", "Shingle oak"]
  pure (genus, species, Just cultivar, commonName)

genPostalCode :: Gen PostalCode
genPostalCode = do
  let genDigit = elements ['0' .. '9']
  PostalCode . read <$> vectorOf 5 genDigit

genFacts :: Gen (Maybe PostalCode, Maybe Double, Maybe Double, Maybe UTCTime, Maybe UTCTime)
genFacts = do
  postalCode <- frequency [(9, Just <$> genPostalCode), (1, pure Nothing)]
  diameter <- fmap abs <$> frequency [(9, Just <$> arbitrary), (1, pure Nothing)]
  height <- fmap abs <$> frequency [(9, Just <$> arbitrary), (1, pure Nothing)]
  datePlanted <- frequency [(9, arbitrary), (1, pure Nothing)]
  dateRemoved <- frequency [(9, arbitrary), (1, pure Nothing)]
  pure (postalCode, diameter, height, datePlanted, dateRemoved)

genTreeId :: Gen Int
genTreeId = do
  let genDigit = elements ['0' .. '9']
  read <$> vectorOf 5 genDigit

mkTree :: IO NFTree
mkTree = do
  nFTreeTreeId <- generate genTreeId
  nFTreeUpdatedAt <- generate arbitrary
  (nFTreeGenus, nFTreeSpecies, nFTreeCultivar, nFTreeCommonName) <- generate genScientificName
  (mNFTreePostalCode, nFTreeDiameter, nFTreeHeight, nFTreeDatePlanted, nFTreeDateRemoved) <- generate genFacts
  let nFTreePostalCode = (\(PostalCode p) -> p) <$> mNFTreePostalCode
  stdGen <- newStdGen
  let (randomBs, _) = genByteString 8 stdGen
      nFTreeSecret = T.pack . show $ hash @_ @SHA1 (encodeUtf8 nFTreeGenus <> encodeUtf8 nFTreeSpecies <> randomBs)
  pure NFTree {..}
