{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty     (ActionM, scotty, get, post, liftAndCatchIO,
                                    rescue, html, param, jsonData)
import Data.Aeson.Types
import Process
import TxtMessage
import Data.Text.Lazy


-- the program will recivie a json object, parse it and then process it
main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = do
      rslt <- jsonData :: ActionM TxtMessage
      (liftAndCatchIO $ do
                          r <- runProcess rslt getResponse
                          putStrLn r
                          return r
                          ) >>= \x -> html $ pack x
