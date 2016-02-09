import Control.Monad.IO.Class (MonadIO)
import Test.Tasty
import Test.Tasty.HUnit
import Database.Persist
import Control.Monad.Reader (ReaderT)
-- import Database.Persist
import Database.Persist.Sql (SqlBackend)

import SDMechanism
import Persist
import Types

import Harness

main :: IO ()
main = defaultMain tests

runDB :: MonadIO m => ReaderT SqlBackend m a -> m a
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

threeMonths :: Assertion
threeMonths = do
    res <- runDB $ do
        insert_ (MechPatron 0 0)
        insert_ (MechProject 0 0)
        newPledge (HR 0) (HA 0)
    res @?= Left InsufficientFunds

    res' <- runDB $ do
        _ <- upsert (MechPatron 3 0) []
        newPledge (HR 0) (HA 0)
    res' @?= Right ()

-- Now, a fun part: setting up a test. This will require doing the
-- runMigrations thing, and creating a pool of connections (minsize: 1),
-- and then initializing (dropping) the data. Ideally tests can run in
-- parallel, which means separate data and a poolsize > 1. Can that be
-- done?


