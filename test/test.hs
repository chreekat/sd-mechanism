{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid ((<>))
import Test.Tasty
import Test.Tasty.Persist.Postgres
import Database.Persist
import Database.Persist.Postgresql

import SDMechanism
import Persist
import Types

import Harness

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = withDB conn migrateMech $ dbTestGroup "input processor"
    [ dbTestCase "nobody is ever overspent" pending
    , dbTestCase "during payout, projects are independent" pending
    , dbTestGroup "patron must support 3 months of pledging" threeMonths
    ]
  where
    conn = PostgresConf str 1
    str = "postgresql:///ignored?"
        <> "host=/home/b/src/Haskell/snowdrift/sd-mechanism/.postgres-work/sockets"

threeMonths :: [DBTestTree]
threeMonths =
    [ dbTestCase "only patron (insufficient)" $
        do insert_ (MechPatron 0 0)
           insert_ (MechProject 0 0)
           newPledge (HR 0) (HA 0)
        `shouldBe` (Left InsufficientFunds)
    , dbTestCase "only patron (sufficient)" $
        do insert_ (MechPatron 3 0)
           insert_ (MechProject 0 0)
           newPledge (HR 0) (HA 0)
        `shouldBe` Right ()
    , dbTestCase "2 patrons (insufficient)" $
        do a <- insert (MechPatron 6 0)
           r <- insert (MechProject 0 0)
           insert_ (Pledge r a)
           insert_ (MechPatron 4 1)
           -- ^ 4 = enough for 1, but not enough for 2
           newPledge (HR 0) (HA 1)
        `shouldBe` Left InsufficientFunds
    , dbTestCase "2 patrons (sufficient)" $
        do a <- insert (MechPatron 6 0)
           r <- insert (MechProject 0 0)
           insert_ (Pledge r a)
           insert_ (MechPatron 6 1)
           newPledge (HR 0) (HA 1)
        `shouldBe` Right ()
    , dbTestCase "many patrons (insufficient)" $
        do as <- mapM (insert . MechPatron 100) [0..2000]
           r <- insert (MechProject 0 0)
           mapM_ (insert_ . Pledge r) as
           insert_ (MechPatron 4000 4000)
           newPledge (HR 0) (HA 4000)
        `shouldBe` Left InsufficientFunds
    , dbTestCase "many patrons (sufficient)" $
        do as <- mapM (insert . MechPatron 100) [0..2000]
           r <- insert (MechProject 0 0)
           mapM_ (insert_ . Pledge r) as
           insert_ (MechPatron 7000 4000)
           newPledge (HR 0) (HA 4000)
        `shouldBe` Right ()
    ]
