{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module DB where

import Data.Aeson.Types
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import GHC.Generics

data Trace r = Trace { trace :: ([Item r], [Item r]) }
                deriving (Show, Read, Generic)



instance (FromField r, ToField r) => Model (Trace r) where
  modelInfo = defaultModelInfo { modelTable = "usersprv" }

data Item r = Answer r | Result String
  deriving (Show, Read, Generic)


connectionInfo = ConnectInfo {connectHost = "localhost",
                              connectPort = 5432,
                              connectUser = "admin",
                              connectPassword = "admin",
                              connectDatabase = "chatbot" }
