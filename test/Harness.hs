module Harness where

import Data.Int
import Database.Persist

import Types

data HPatron = HA Int
data HProject = HR Int

instance ToMechPatron HPatron where
    mechPatron (HA i) = i
    toExternalPatron = HA

instance ToMechProject HProject where
    mechProject (HR i) = i
    toExternalProject = HR
