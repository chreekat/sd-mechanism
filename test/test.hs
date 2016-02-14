{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import Data.Pool (Pool, destroyAllResources)
import Database.Persist
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT(..))
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlPool)
import Database.PostgreSQL.Simple (execute_, connectPostgreSQL, close)

import SDMechanism
import Persist
import Types

import Harness

main :: IO ()
main = defaultMain tests

-- | *Some* kind of MonadLogger is needed to satisfy the types of the
-- library methods used to create a database pool. Since logging isn't
-- actually used yet, I'm using NoLoggingT.
type DBAssertion = SqlPersistT (NoLoggingT IO) ()

pending :: DBAssertion
pending = liftIO (assertFailure "(test is pending)")

shouldBe :: (Eq a, Show a) => a -> a -> DBAssertion
a `shouldBe` b = liftIO (a @?= b)

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

type DBTestTree = IO (Pool SqlBackend) -> TestTree

-- | Creates a pool of temp databases conns and passes it to the given tree
-- of db tests, returning a regular TestTree back to the tasty machinery.
dbTestGroup :: TestName -> [DBTestTree] -> TestTree
dbTestGroup label = withResource mkTempDBPool cleanseDatabase . buildDbTree label
  where
    mkTempDBPool = runNoLoggingT $ do
        pgExecute z "create database blablab"
        createPostgresqlPool str 10
    z = "postgresql:///postgres?host=/home/b/src/Haskell/snowdrift/sd-mechanism/postgres/sockets"
    str = "postgresql:///blablab?host=/home/b/src/Haskell/snowdrift/sd-mechanism/postgres/sockets"
    cleanseDatabase pool = do
        destroyAllResources pool
        pgExecute z "drop database blablab"
        return ()
    pgExecute constr q = void . liftIO $ bracket (connectPostgreSQL constr)
                                                 close
                                                 (\c -> execute_ c q)
-- | Given a group of db tests, build a callback usable by withResource.
-- The sqlbackend pool will need to be threaded to each Assertion (== IO
-- ()) that defines a test.
buildDbTree :: TestName -> [DBTestTree] -> IO (Pool SqlBackend) -> TestTree
buildDbTree label dbtree mpool = testGroup label (map ($ mpool) dbtree)

dbTestCase :: TestName -> DBAssertion -> DBTestTree
dbTestCase label stmt mpool = testCase label $ do
    pool <- mpool
    runNoLoggingT (runSqlPool stmt pool)
