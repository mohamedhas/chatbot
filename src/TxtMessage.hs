{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module TxtMessage where

import Data.Aeson.Types
import GHC.Generics

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

instance FromJSON Entity
instance FromJSON NLP
instance FromJSON TxtMessage -- parse json object to msg object
