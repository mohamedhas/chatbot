{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty     (ActionM, scotty, get, post, rescue, html, param, jsonData)
import Data.Aeson.Types
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Reader
import DB

data St = St {
  previlege :: Prv
}

type Process m a = ReadT St (ExceptT a m)

getPrevilegeDB :: LIO ACC ( Labeled ACC Int ) -> LIOState ACC -> IO Prv

runProcess :: (Show a) =>  Process m a -> TxtMessage -> IO ()
runProcess p txtMsg= do
  prv <- getPrevilegeDB (label H (userid txtMsg))
                               (LIOState {lioLabel = H, lioClearance = H})
  rslt <- ( runExceptT $ runReaderT (St prv) p)
  putStrLn "test"


getContext :: ( Labeled ACC TxtMessage ) -> Process IO (Lio ACC Context)
getContext msg = do
  prv     <- asks previlege
  message <- liftLIO (unlabelP prv msg)
  userid


getLastContxt :: Labeled ACC UserId -> Process IO (Lio ACC Context)
getLastContxt id = do



data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show, Generic)

data TxtMessage = TxtMessage {
  msg :: String,
  userid :: Int,
  msgid :: Int,
  time :: Int,
  nlp :: NLP
} deriving (Show, Generic)

data NLP = NLP {
  entities :: [Entity]
} deriving (Show, Generic)

data Entity = Entity {
  entity :: String,
  confidence :: Double,
  value :: String
} deriving (Show, Generic)

data Context = QuestContxt {trace :: (Trace String) }   |
               Request
               deriving (Show, Generic)
{-
               { msg :: TxtMessage Entity,
                          :: Entity
                       } 
-}
--               Command TxtMessage

getLastContxt :: Context

loadContxt :: TxtMessage ->

instance FromJSON Point

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = do
        rslt <- jsonData :: ActionM Point
        liftIO $ putStrLn $ show rslt
