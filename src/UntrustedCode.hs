module UntrustedCode where

import Lattice
import LIO.Labeled
import LIO.Core
import Data.Aeson.Types
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple.Internal
import Database.PostgreSQL.Simple
import LIO.TCB
import Context
import DB
{- this module use TCB so it must be fully reviewed -}

getLastContextDB ::  ( Labeled ACC UserId ) -> LIO ACC Context
getLastContextDB ident = do
    iden  <-  unlabel ident
    ioTCB $ getDataBasedOnUserId iden read
