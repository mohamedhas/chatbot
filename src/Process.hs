module Process where

import Data.Aeson.Types
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Except
import LIO.Labeled
import LIO.Core
import LIO.TCB
import Lattice
import Questionnaire
import Context
import DB
import User
import TxtMessage
import Replay (run, Replay, ReplayT, Trace,
               emptyTrace, addAnswer, Item)

data St = St {
  --previlege :: Prv,
  txtmsg :: Labeled ACC TxtMessage
}

liftLio :: LIO ACC a -> Process a
liftLio computation = lift $ lift computation

getUserId :: Labeled ACC TxtMessage -> LIO ACC (Labeled ACC UserId)
getUserId msg = do
  rslt <- unlabel msg
  l <- getClearance
  label l (userid rslt)

getField :: (b -> a) -> Labeled ACC b -> LIO ACC (Labeled ACC a)
getField f v = do
  rslt <- unlabel v
  l <- getClearance
  label l (f rslt)


-- a Monad that process the messages then it try to get a the best response
-- TODO Manage Exceptions in a better way
type Process = ReaderT St (ExceptT String (LIO ACC))

nonUnderstandingHandler = throwError "the admin will join you soon"

-- a function that tries to get a response
getResponse :: Process String
getResponse = do
      env <- ask
      message <- lift $ lift $ unlabel (txtmsg env)
      let entity_ = entities $ nlp message -- getting the nlp result
      --TODO improve code quality
      case entity_ of
        [] -> nonUnderstandingHandler -- if the nlp failed to understand the msg the admin will respond to the request
        xs -> tryWithDiffrentEntities xs  -- else we try to get the base response based on what state 'context' we are in
      return "k"

-- nlp can produce a list of possible meaning, so we will try them all for now.
-- the algorithm can be improved
tryWithDiffrentEntities :: [Entity] -> Process String
tryWithDiffrentEntities [] = nonUnderstandingHandler
tryWithDiffrentEntities (x:xs) = do
    env <- ask
    usrId <- lift $ lift (getUserId (txtmsg env ) )
    lastCtxt <- lift $ lift $ getLastContextDB usrId -- get Last State of the user
    --TODO improve code quality
    case lastCtxt of
      QuestionnaireCtx -> if (isRequestEntities $ entity x) then nonUnderstandingHandler -- we test if msg fit in the last state we were in
                              else runQuestionnaire
      Request -> case (lookup (entity x) requestResponse) of
          Nothing   -> tryWithDiffrentEntities xs
          Just rslt -> return rslt


runQuestionnaire :: Process String
runQuestionnaire = do
  env <- ask
  usrId    <- liftLio (getUserId (txtmsg env ) )
  qstTrace <- liftLio $ getTraceDB usrId
  message  <- liftLio $ unlabel (txtmsg env)
  lastCtxt <- liftLio (getLastContextObjDB usrId >>=
                                        \x -> getClearance >>= \y -> label y x)
  let trace = parseTrace $ addQstAnswer (msg message) qstTrace
  rslt <- lift $ lift $ run (questionnaireExample lastCtxt) trace
  case rslt of
    Left rslt  -> return $ fst rslt
    Right rslt -> nonUnderstandingHandler



{-
runQuestionnaire :: Process String
runQuestionnaire = do
  env <- ask
  usrId <- lift $ lift (getUserId (txtmsg env ) )
  qstTrace <- lift $ lift $ getTraceDB usrId
  message <- lift $ lift $ unlabel (txtmsg env)
  let trace = parseTrace $ addQstAnswer (msg message) qstTrace
  rslt <- lift $ lift $ run questionnaireExample trace
  case rslt of
    Left rslt  -> return $ fst rslt
    Right rslt -> nonUnderstandingHandler
-}

runProcess :: TxtMessage -> Process String -> IO String
runProcess msg process = do
  let lioSt = LIOState {lioLabel = H, lioClearance = H}
  uId <- evalLIO (label L (userid msg)) lioSt
  l   <- evalLIO (getPrevilegeDB uId ) lioSt
  let userState = LIOState {lioLabel = H, lioClearance = l}
  st   <- evalLIO (label l msg ) userState
  rslt <- evalLIO  ( runExceptT (runReaderT process (St st)) ) userState
  case rslt of
    Left rslt  -> return rslt
    Right rslt -> return rslt
