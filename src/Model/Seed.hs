module Model.Seed where

import Control.Monad (void)
import Data.Password.Bcrypt (PasswordHash (..), hashPassword, mkPassword)
import Database.Esqueleto.Experimental
import Model.Model (RegisteredUser (..))

seedDatabase :: SqlPersistT IO ()
seedDatabase = do
  PasswordHash hashedPassword <- hashPassword $ mkPassword "chiroptical"
  void $
    insertUnique
      ( RegisteredUser
          { registeredUserUsername = "chiroptical"
          , registeredUserEmail = "chiroptical@email.com"
          , registeredUserHashedPassword = hashedPassword
          }
      )
