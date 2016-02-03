module Types where

import Persist

class ToMechPatron a where
    mechPatron :: a -> MechPatronId
    toPatron :: MechPatronId -> a

class ToMechProject a where
    mechProject :: a -> MechProjectId
    toProject :: MechProjectId -> a
