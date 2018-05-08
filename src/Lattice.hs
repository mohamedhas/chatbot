{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE Safe #-}

-- | Encodes a security lattice.


module Lattice where

import LIO.Label
import Data.Typeable


data ACC =   L
           | H
            deriving (Eq, Show, Read, Typeable)


instance Label ACC where

  lub (H) (H) = H
  lub a b     = L

  glb L L     = L
  glb a b     = H

  canFlowTo (L) (H) = False
  canFlowTo a b     = True


data Prv = AdminPriv
          | UserPriv
          deriving (Show, Typeable, Read)

instance SpeaksFor Prv where
  speaksFor (UserPriv) (AdminPriv) = False
  speaksFor a b                    = True

instance PrivDesc ACC Prv where

  downgradeP p l  = L

  canFlowToP AdminPriv L H = True
  canFlowToP p l1 l2 = downgradeP p l1 `canFlowTo` l2
