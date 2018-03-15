{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module User where

import GHC.Generics
import Lattice
import LIO.Labeled
import LIO.Core
import Data.Aeson.Types
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple
import LIO.TCB
import DB

data UsersPrv = UsersPrv {
  dbkey :: !DBKey,
  usrid :: !Int,
  previlege :: String
} deriving (Show, Generic)


instance Model UsersPrv where
  modelInfo = defaultModelInfo { modelTable = "usersprv" }


getPrevilegeDB ::  ( Labeled ACC UserId ) -> LIO ACC ACC
getPrevilegeDB ident = do
    id  <-  unlabel ident
    ioTCB $ getDataBasedOnUserId id (read . previlege)
