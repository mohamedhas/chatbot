{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module DB where

import Data.Aeson.Types
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple
import GHC.Generics


type UserId  = Int

connectionInfo = ConnectInfo {connectHost = "localhost",
                              connectPort = 5432,
                              connectUser = "admin",
                              connectPassword = "admin",
                              connectDatabase = "chatbot" }
