{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty     (ActionM, scotty, get, post, liftAndCatchIO,
                                    rescue, html, param, jsonData)
import Data.Aeson.Types
{-
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
-}
import Process
import TxtMessage
import Data.Text.Lazy
-- the

-- the program will recivie a json object, parse it and then it will process it
main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = do
        rslt <- jsonData :: ActionM TxtMessage
        (liftAndCatchIO $ runProcess rslt getResponse) >>= \x -> html $ pack x
