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
import LIO.TCB
import DB



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


getTraceDB ::  ( Labeled ACC UserId ) -> LIO ACC (Trace String)
getTraceDB ident = do
    id  <-  unlabel ident
    ioTCB $ getDataBasedOnUserId id parseTrace

type Questionnaire = Replay String String String

questionnaireExample = do
  ask "hello, can i ask you some questions ?"
  ask "which service do you use the most ?"
  ask "can you rate this service ?"
  ask "thank you"
