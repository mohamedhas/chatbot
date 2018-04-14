{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Replay (run, ask, io, Replay, ReplayT, Trace,
               emptyTrace, addAnswer, Item) where

import System.IO.Unsafe
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Except
import Data.Text.Lazy (Text)
import DB
import LIO.Core
import Lattice

{- i ve transformed the trace to a tuple of "(originalTrace, newTrace)" -}
type Trace r q = ([Item r q], [Item r q])

data Item r q = Answer r (Maybe q) | Result String | Approval Bool
  deriving (Show, Read)


emptyTrace :: Trace r q
emptyTrace = ([], [])

addAnswer :: Trace r q -> r -> Trace r q
addAnswer (trace, output) re = (trace ++ [(Answer re Nothing)], output)

type Replay q r = ReplayT IO q r

type ReplayT m q r  = StateT (Trace r q) (ExceptT (q, Trace r q) m)

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
    ((Answer aswr Nothing):trace, output)   -> error $ "undifined"
    ([], output)                    -> helperFunction i
    ((Result rslt):trace, output)   -> put (trace, output ++ [(Result rslt)]) >>=
                                      \_ -> return $ read rslt
    ((Approval rslt):trace, output) -> error $ "undifined"

-- io :: (Show a, Read a) => IO a -> Replay q r a
-- io i = liftR i
--y = \f -> (\x -> f (x x)) (\x -> f (x x))

io :: (Show a, Read a) => LIO ACC a -> ReplayT (LIO ACC) q r a
io i = liftR i

verifyAnswer :: (Monad m, Read r) => (r -> Bool) -> ReplayT m q r ()
verifyAnswer pred = do
    trace' <- get
    case trace' of
      ((Approval rslt):trace, output) -> put (trace, output) >>= return
    --((Answer answr (Just qst)):trace, output)  ->
      ([], (Answer answr (Just qst)):output) ->
            if (pred answr) then put (([], output ++ [(Approval True)]))
              else throwError (qst, (output, []))


ask :: (Monad m, Show q, Show r) => q -> ReplayT m q r r
ask qst = do
  trace' <- get
  case trace' of
    ((Answer answr Nothing):trace, output) ->
        put (trace, output ++ [(Answer answr $ Just qst)]) >>= \_ -> return answr
    ((Answer answr qs):trace, output) ->
        put (trace, output ++ [(Answer answr qs)]) >>= \_ -> return answr
    ((Result str):trace, output)   -> throwError (qst, (output, []))
    ([], output)                   -> throwError (qst, (output, []))
    ((Approval rslt):trace, output) -> error $ "undifined"
    (trace, output) -> error $ ((show trace) ++ (show output))

run :: (Monad m) => ReplayT m q r a -> Trace r q -> m (Either (q, Trace r q) a)
run replay trace = runExceptT (evalStateT replay trace)
