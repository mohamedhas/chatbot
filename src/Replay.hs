{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Replay (run, ask, io, Replay, ReplayT, Trace, emptyTrace, addAnswer) where

import System.IO.Unsafe
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Except
import Web.Scotty     (ActionM, scotty, get, post, rescue, html, param)
import Data.Text.Lazy (Text)
import DB

{- i ve transformed the trace to a tuple of "(originalTrace, newTrace)" -}

emptyTrace :: Trace r
emptyTrace = ([], [])

addAnswer :: Trace r -> r -> Trace r
addAnswer (trace, output) re = (trace ++ [(Answer re)], output)

type Replay q r  = ReplayT IO q r

type ReplayT m q r  = StateT (Trace r) (ExceptT (q, Trace r) m)

helperFunction :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
helperFunction i = do
  ((rslt, item)) <-  lift (lift (i >>= \x -> return (x, Result $ show x)))
  trace' <- get
  case trace' of
    ([], output)   -> put ([], output ++ [item])
    (x:xs, output) -> put (xs, output ++ [item])
  return rslt

liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR i = do
  trace' <- get
  case trace' of
    ((Answer aswr):trace, output) -> error $ "undifined"
    ([], output)                  -> helperFunction i
    ((Result rslt):trace, output) -> put (trace, output ++ [(Result rslt)]) >>=
                                      \_ -> return $ read rslt

io :: (Show a, Read a) => IO a -> Replay q r a
io i = liftR i

ask :: (Monad m) => q -> ReplayT m q r r
ask qst = do
  trace' <- get
  case trace' of
    ((Answer answr):trace, output) ->
        put (trace, output ++ [(Answer answr)]) >>= \_ -> return answr
    ((Result str):trace, output)   -> throwError (qst, (output, []))
    ([], output)                   -> throwError (qst, (output, []))


run :: (Monad m) => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
run replay trace = runExceptT (evalStateT replay trace)
