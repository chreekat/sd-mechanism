import Test.Tasty
import Test.Tasty.HUnit
import Database.Persist

import SDMechanism
import Persist
import Types

import Harness

main :: IO ()
main = defaultMain tests

runDB = undefined

pending :: Assertion
pending = assertFailure "(test is pending)"

tests :: TestTree
tests = testGroup "input processor"
    [ testCase "nobody is ever overspent" pending
    , testCase "during payout, projects are independent" pending
    , testCase "doesn't make a pledge if funds are insufficient" pending
    , testCase "patron must support 3 months of pledging" threeMonths
    ]

threeMonths = do
    res <- runDB $ do
        otherPatrons <- mapM insert (replicate 5 (MechPatron 1000))
        patron <- insert (MechPatron 0)
        project <- insert (MechProject 0)
        mapM_ (insert . Pledge project) otherPatrons

        newPledge project patron
    res @=? InsufficientFunds

-- Now, a fun part: setting up a test. This will require doing the
-- runMigrations thing, and creating a pool of connections (minsize: 1),
-- and then initializing (dropping) the data. Ideally tests can run in
-- parallel, which means separate data and a poolsize > 1. Can that be
-- done?
