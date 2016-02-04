module Types where

import Data.Int
import Data.Text (Text)

class ToMechPatron a where
    mechPatron :: a -> Int64
    toPatron :: Int64 -> a

class ToMechProject a where
    mechProject :: a -> Int64
    toProject :: Int64 -> a


data MechError = Internal IMechError | External EMechError
data IMechError = BadKey Text
data EMechError = InsufficientFunds | ExistingPledge | NoSuchProject | NoSuchPatron
