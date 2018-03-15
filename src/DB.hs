{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module DB where


import GHC.Generics
import Lattice
import LIO.Labeled
import LIO.Core
import Data.Aeson.Types
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple

type UserId  = Int

getDataBasedOnUserId :: (Model a) => UserId -> (a -> b) -> IO b
getDataBasedOnUserId id parse =  do
             cnx <- connect connectionInfo
             list <- dbSelect cnx $ addWhere "user_id = ?" (Only id) $
                  (modelDBSelect)
             case list of
                []     -> return $ error "undifined"
                (x:xs) -> return $ parse x

connectionInfo = ConnectInfo {connectHost = "localhost",
                              connectPort = 5432,
                              connectUser = "admin",
                              connectPassword = "admin",
                              connectDatabase = "chatbot" }
