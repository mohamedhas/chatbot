module User where

import GHC.Generics
import Lattice
import LIO.Labeled

data UserP = Admin {
  userId :: String
  previlege :: String
} deriving (Generic)

getPrevilegeDB ::
