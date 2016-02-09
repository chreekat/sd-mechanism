module Types where

class ToMechPatron a where
    mechPatron :: a -> Int
    toExternalPatron :: Int -> a

class ToMechProject a where
    mechProject :: a -> Int
    toExternalProject :: Int -> a

data MechError = InsufficientFunds | ExistingPledge | NoSuchProject | NoSuchPatron | ExistingPatron
    deriving (Show, Eq)
