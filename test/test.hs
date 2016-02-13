import Test.Tasty
import Test.Tasty.HUnit
import Database.Persist
import Control.Monad.Logger (runStderrLoggingT, LoggingT)
import Control.Monad.IO.Class (liftIO)
-- import Database.Persist
import Database.Persist.Sql (SqlBackend, close', SqlPersistT, runSqlConn)

import SDMechanism
import Persist
import Types

import Harness

main :: IO ()
main = defaultMain tests

type DBAssertion = SqlPersistT (LoggingT IO) ()

pending :: DBAssertion
pending = liftIO $ assertFailure "(test is pending)"

shouldBe :: (Eq a, Show a) => a -> a -> DBAssertion
a `shouldBe` b = liftIO $ a @?= b

tests :: TestTree
tests = dbTestGroup "input processor"
    [ dbTestCase "nobody is ever overspent" pending
    , dbTestCase "during payout, projects are independent" pending
    , dbTestCase "doesn't make a pledge if funds are insufficient" pending
    , dbTestCase "patron must support 3 months of pledging" threeMonths
    ]

threeMonths :: DBAssertion
threeMonths = do
    res <- do
        insert_ (MechPatron 0 0)
        insert_ (MechProject 0 0)
        newPledge (HR 0) (HA 0)
    res `shouldBe` Left InsufficientFunds

    res' <- do
        _ <- upsert (MechPatron 3 0) []
        newPledge (HR 0) (HA 0)
    res' `shouldBe` Right ()

type DBTestTree = IO SqlBackend -> TestTree

-- | Creates a temp database and passes it to the given tree of db tests,
-- returning a regular TestTree back to the tasty machinery.
dbTestGroup :: TestName -> [DBTestTree] -> TestTree
dbTestGroup label = withResource mkTempDatabase close' . buildDbTree label

mkTempDatabase :: IO SqlBackend
mkTempDatabase = undefined

-- | Given a group of db tests, build a callback usable by withResource.
-- The sqlbackend will need to be threaded to each Assertion (== IO ())
-- that defines a test.
buildDbTree :: TestName -> [DBTestTree] -> IO SqlBackend -> TestTree
buildDbTree label dbtree mdb = testGroup label (map ($ mdb) dbtree)

dbTestCase :: TestName -> DBAssertion -> DBTestTree
dbTestCase label stmt mdb = testCase label $ do
    db <- mdb
    runStderrLoggingT (runSqlConn stmt db)
