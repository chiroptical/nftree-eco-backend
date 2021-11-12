module Model.Seed (
  unsafeDropRegisteredUsers,
  seedDatabase,
) where

import Control.Monad (void)
import Crypto.Hash
import Data.Password.Bcrypt (PasswordHash (..), hashPassword, mkPassword)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Database.Esqueleto.Experimental
import Model.Model (NFTree (..), RegisteredUser (..))
import System.Random
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

  americanBasswood <- liftIO $ mkTree "American Basswood" "Tilia americana"
  void $ insertUnique americanBasswood

  americanElm <- liftIO $ mkTree "American Elm" "Ulmus americana"
  void $ insertUnique americanElm

  commonHackberry <- liftIO $ mkTree "Common Hackberry" "Celtis occidentalis"
  void $ insertUnique commonHackberry

  easternWhitePine <- liftIO $ mkTree "Eastern White Pine" "Pinus strobus"
  void $ insertUnique easternWhitePine

mkTree :: MonadIO m => Text -> Text -> m NFTree
mkTree common latin = do
  stdGen <- newStdGen
  let (randomBs, _) = genByteString 8 stdGen
      secret = T.pack . show $ hash @_ @SHA1 (encodeUtf8 common <> encodeUtf8 latin <> randomBs)
  pure
    NFTree
      { nFTreeCommonName = common
      , nFTreeLatinName = latin
      , nFTreeSecret = secret
      }
