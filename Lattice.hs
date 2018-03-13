{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE Safe #-}

-- | Encodes a security lattice.


module Lattice where

import LIO.Label
import Data.Typeable

-- | Label for public data
data L = MkL deriving (Eq, Show, Read, Typeable)
-- | Label for secrets
data H = MkH deriving (Eq, Show, Read, Typeable)

-- Pablo's trick to avoid instances
-- Define a super-class
-- | Type class used to avoid arbitrary instances by attackers (Pablo's trick)
class CanFlowTo l l' where

-- | Type class encoding security lattices
class CanFlowTo l l' => Less l l' where

instance CanFlowTo L L where
instance CanFlowTo L H where
instance CanFlowTo H H where

instance Less L L where
instance Less L H where
instance Less H H where

instance Label H where
instance Label L where

data AdminPriv = AdminPriv deriving (Show, Typeable)
data UserPriv = UserPriv deriving (Show, Typeable)

instance SpeaksFor AdminPriv where
  speaksFor a b = True
instance SpeaksFor UserPriv where
  speaksFor a b = True

instance PrivDesc H AdminPriv where
  downgradeP p l = MkH
instance PrivDesc L AdminPriv where
  downgradeP p l = MkL

instance PrivDesc L UserPriv where
  downgradeP p l = MkL
--instance PrivDesc L AdminPriv where
