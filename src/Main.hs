{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty     (ActionM, scotty, get, post, rescue, html, param, jsonData)
import Data.Aeson.Types
import GHC.Generics
import Control.Monad.IO.Class



data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show, Generic)

data TxtMessage = TxtMessage {
  msg :: String,
  userId :: String,
  msgId :: String,
  time :: Int,
  nlp :: NLP
} deriving (Show, Generic)

data NLP = NLP {
  entities :: [Entity]
}

data Entity = Entity {
  entity :: String,
  confidence :: Double,
  value :: String
} deriving (Show, Generic)

data Context = QuestContxt (Trace String)   |
               Request TxtMessage Entity    
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
