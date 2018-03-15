{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty     (ActionM, scotty, get, post, rescue, html, param, jsonData)
import Data.Aeson.Types
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import LIO.Labeled
import LIO.Core
import LIO.TCB
import Lattice
import DB

-- the
data St = St {
  previlege :: Prv,
  txtmsg :: Labeled ACC TxtMessage
}

-- a Monad that process the messages then it try to get a the best response
-- TODO Manage Exceptions in a better way
type Process = ReaderT St (ExceptT String (LIO ACC))

-- a function that tries to get a response
getResponse :: Process String
getResponse = do
      env <- ask
      message <- lift $ lift $ unlabel (txtmsg env)
      let entity_ = entities $ nlp message -- getting the nlp result
      --TODO improve code quality
      case entity_ of
        [] ->  throwError "the admin will join you soon" -- if the nlp failed to understand the msg the admin will respond to the request
        xs -> case tryWithDiffrentEntities of  -- else we try to get the base response based on what state 'context' we are in
      return "k"

-- nlp can produce a list of possible meaning, so we will try them all for now.
-- the algorithm can be improved
tryWithDiffrentEntities :: Labeled ACC [Entity] -> Process String
tryWithDiffrentEntities [] = throwError "the admin will join you soon"
tryWithDiffrentEntities (x:xs) = do
    env <- ask
    lastCtxt <- getLastContextDB $ userid $ lift $ lift $ unlabel $ txtmsg env
    --TODO improve code quality
    case lastCtxt of -- we test if msg fit in the last state we were in
      QuestionnaireCtx ->
      Request -> case (lookup (entity x) requestResponse) of
          Nothing   -> tryWithDiffrentEntities $ label xs
          Just rslt -> rslt

runQuestionnaire :: Process String
runQuestionnaire = do
  env <- ask
  trace <- getTraceDB $ userid $ lift $ lift $ unlabel $ txtmsg env

-- Recived message
data TxtMessage = TxtMessage {
  msg :: String,
  userid :: Int,
  msgid :: Int,
  time :: Int,
  nlp :: NLP
} deriving (Show, Generic)

-- nlp Result
data NLP = NLP {
  entities :: [Entity]
} deriving (Show, Generic)

data Entity = Entity {
  entity :: String, -- entity name
  confidence :: Double, -- correctness ratio
  value :: String
} deriving (Show, Generic)



          --message <- lift $ liftLIO (unlabelP prv msg)

{-
runProcess :: (Show a) =>  Process m a -> UserId -> IO ()
runProcess p txtMsg= do
  prv <- getPrevilegeDB (label H (userid txtMsg))
                               (LIOState {lioLabel = H, lioClearance = H})
  rslt <- ( runExceptT $ runReaderT (St prv) p)
  putStrLn "test"


-}


main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = do
        --rslt <- jsonData :: ActionM Point
        liftIO $ putStrLn "hello" -- $ show rslt
