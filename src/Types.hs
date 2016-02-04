module Types where

import Data.Int
import Data.Text (Text)

class ToMechPatron a where
    mechPatron :: a -> Int
    toExternalPatron :: Int -> a

class ToMechProject a where
    mechProject :: a -> Int
    toExternalProject :: Int -> a


data MechError = InsufficientFunds | ExistingPledge | NoSuchProject | NoSuchPatron
    deriving (Eq)
