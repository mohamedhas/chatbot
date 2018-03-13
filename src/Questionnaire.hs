{-# LANGUAGE OverloadedStrings #-}
module Questionnaire where

import Replay (run, ask, io, Replay, ReplayT, Trace, emptyTrace, addAnswer)


type Questionnaire = Replay String String String
