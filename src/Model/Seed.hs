module Model.Seed where

import Control.Monad (void)
import Data.Password.Bcrypt (PasswordHash (..), hashPassword, mkPassword)
import Database.Esqueleto.Experimental
import Model.Model (RegisteredUser (..))
import UnliftIO (MonadIO)

unsafeDropRegisteredUsers :: MonadIO m => SqlPersistT m [Single Int]
unsafeDropRegisteredUsers = rawSql "DROP TABLE IF EXISTS registered_users" []

seedDatabase :: SqlPersistT IO ()
seedDatabase = do
  PasswordHash hashedPassword <- hashPassword $ mkPassword "chiroptical"
  void $
    insertUnique
      ( RegisteredUser
          { registeredUserEmail = "chiroptical@email.com"
          , registeredUserHashedPassword = hashedPassword
          }
      )
