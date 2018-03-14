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
import DB

data UsersPrv = UsersPrv {
  dbkey :: !DBKey,
  userid :: !Int,
  previlege :: String
} deriving (Show, Generic)



instance Model UsersPrv where
  modelInfo = defaultModelInfo { modelTable = "usersprv" }

getPrevilegeDB :: LIO ACC ( Labeled ACC Int ) -> LIOState ACC -> IO Prv--(Maybe String)
getPrevilegeDB ident st = do
    id <- evalLIO (ident >>= \x -> unlabel x) st
    cnx <- connect connectionInfo
    list <- dbSelect cnx $ addWhere "userid = ?" (Only id) $
                      (modelDBSelect :: DBSelect UsersPrv)
    case list of
      []     -> return $ error "undifined"
      (x:xs) -> return $ read (previlege x)


{-
getUserPrevilege :: Labeled ACC Int -> LIO ACC (IO Prv)
getPrevilegeDB ident st = do
    id <- unlabel ident
    return $ ( do
        cnx <- connect connectionInfo
        dbSelect
        dbSelect cnx $ addWhere_  ()"userId = " ++ show id) modelDBSelect --(mkDBRef (UsersPrv { userId = id}) )
-}
