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
import Context
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
                modifyTrace (addAnswer (parseTrace (trace qstTrace)) answr) qstTrace

addQstResult :: String -> QstTrace -> QstTrace
addQstResult answr qstTrace =
  modifyTrace (read ((read (trace qstTrace)) ++
                                    (read $ "Result " ++ answr) )) qstTrace

parseItemList :: String -> [(Item String)]
parseItemList str = read str

parseTrace :: String -> Trace String
parseTrace qstTrace = read qstTrace --( (parseItemList (trace qstTrace)), [])

instance Model QstTrace where
  modelInfo = defaultModelInfo { modelTable = "qst_trace" }

clearTrace :: Labeled ACC QstTrace -> LIO ACC (Either ValidationError Bool)
clearTrace trace = do
    trace' <- unlabel trace
    ioTCB $ connect connectionInfo >>= \x -> destroy x trace'

getTraceDB ::  ( Labeled ACC UserId ) -> LIO ACC QstTrace
getTraceDB ident = do
    id  <-  unlabel ident
    ioTCB $ getDataBasedOnUserId id (\x -> x)

saveTrace ::  QstTrace -> LIO ACC QstTrace
saveTrace ctx = do
  ioTCB $ connect connectionInfo >>= \x -> save x ctx -- >>= \y -> return $ show y

type Questionnaire = ReplayT (LIO ACC) String String String

questionnaireExample :: Labeled ACC LastContext -> Questionnaire
questionnaireExample lastCtxt = do
  ask "start"
  ask "hello, can i ask you some questions ?"
  ask "which service do you use the most ?"
  ask "can you rate this service ?"
  io (modifyLastContext lastCtxt)
  ask "thank you"
