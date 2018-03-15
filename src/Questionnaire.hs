{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Questionnaire where

import Replay (run, ask, io, Replay, ReplayT, Trace, emptyTrace, addAnswer, Item)
import GHC.Generics
import Lattice
import LIO.Labeled
import LIO.Core
import Data.Aeson.Types
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple
import DB
import LIO.TCB


data QstTrace = QstTrace {
                   dbkey    :: !DBKey,
                   qst_name :: String,
                   user_id  :: UserId,
                   trace    :: String
                }  deriving (Show, Generic)


modifyTrace :: Trace String -> QstTrace -> QstTrace
modifyTrace answr qstTrace =
      qstTrace { trace = show answr }

addQstAnswer :: String -> QstTrace -> QstTrace
addQstAnswer answr qstTrace =
                modifyTrace (addAnswer (read (trace qstTrace)) answr) qstTrace

addQstResult :: String -> QstTrace -> QstTrace
addQstResult answr qstTrace =
  modifyTrace (read ((read (trace qstTrace)) ++
                                    (read $ "Result " ++ answr) )) qstTrace
{-
parseTrace :: QstTrace -> Trace String
parseTrace qstTrace = ((map (read) (trace qstTrace)), [])
-}

parseTrace :: QstTrace -> Trace String
parseTrace qstTrace = ( (read (trace qstTrace)), [])

instance Model QstTrace where
  modelInfo = defaultModelInfo { modelTable = "qst_trace" }

{-}
getTraceDB :: LIO ACC ( Labeled ACC UserId ) -> LIOState ACC -> LIO ACC (Trace String)--(Maybe String)
getTraceDB ident st = do
    id <- evalLIO (ident >>= \x -> unlabel x) st
    cnx <- connect connectionInfo
    list <- dbSelect cnx $ addWhere "user_id = ?" (Only id) $
                      (modelDBSelect :: DBSelect QstTrace)
    case list of
      []     -> return $ error "undifined"
      (x:xs) -> return $ parseTrace x

      ( do cnx <- connect connectionInfo
                   list <- dbSelect cnx $ addWhere "user_id = ?" (Only id) $
                        (modelDBSelect :: DBSelect QstTrace)
                   case list of
                      []     -> return $ error "undifined"
                      (x:xs) -> return $ parseTrace x )
-}


getTraceDB ::  ( Labeled ACC UserId ) -> LIO ACC (Trace String)--(Maybe String)
getTraceDB ident = do
    id  <-  unlabel ident
    ioTCB $ getDataBasedOnUserId id parseTrace

type Questionnaire = Replay String String String
