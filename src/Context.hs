{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Context where

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


data LastContext = LastContext {
  dbkey   :: !DBKey,
  user_id :: !Int,
  context :: String
} deriving (Show, Generic)

data Context = QuestionnaireCtx | Request deriving (Show, Read)


-- TODO Implement a database for the requestEntities and requestResponse
-- requestEntities represent the requests that the bot can answer
requestEntities = ["food", "wifi password", "location"]
requestResponse = [("food", "food is in the kitchen at 2nd floor"),
                ("wifi password", "12345678"),
                ("location", "pleas check the map in the link www.test.com/map")]

isRequestEntities :: String -> Bool
isRequestEntities entty = case (lookup entty requestResponse) of
                            Nothing -> False
                            _       -> True

modifyLastContext :: Labeled ACC LastContext -> Prv -> LIO ACC String
modifyLastContext lastContext p = do
  ctx <- unlabelP (PrivTCB p) lastContext
  ioTCB $ connect connectionInfo >>= \x -> save x (ctx {context = "Request"})
                                            >>= \y -> return $ show y

modifyLastContext_ :: Labeled ACC LastContext -> LIO ACC String
modifyLastContext_ lastContext = do
  ctx <- unlabel lastContext
  ioTCB $ connect connectionInfo >>= \x -> save x (ctx {context = "QuestionnaireCtx"})
                                            >>= \y -> return $ show y

getLastContextDB ::  ( Labeled ACC UserId ) -> LIO ACC Context
getLastContextDB ident = do
    iden  <-  unlabel ident
    ioTCB $ getDataBasedOnUserId iden (\x -> read (context x) )

getLastContextObjDB ::  ( Labeled ACC UserId ) -> LIO ACC (Labeled ACC LastContext)
getLastContextObjDB ident = do
    iden  <-  unlabel ident
    lastCtxt <- ioTCB $ getDataBasedOnUserId iden id
    p <- label H lastCtxt
    return p


instance Model LastContext where
  modelInfo = defaultModelInfo { modelTable = "last_context" }
