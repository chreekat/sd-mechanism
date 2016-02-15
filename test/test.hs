{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import Data.Pool (Pool, destroyAllResources)
import Database.Persist
import Data.Monoid ((<>))
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT(..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql
        (SqlBackend, SqlPersistT, runSqlPool, transactionUndo, runMigrationSilent)
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
type DBTestTree = IO (Pool SqlBackend) -> TestTree

pending :: MonadIO m => m ()
pending = liftIO (assertFailure "(test is pending)")

shouldBe :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
ma `shouldBe` b = ma >>= (liftIO . (@?= b))

dbTestCase :: TestName -> DBAssertion -> DBTestTree
dbTestCase label stmt mpool = testCase label $ do
    pool <- mpool
    runNoLoggingT (runSqlPool (stmt >> transactionUndo) pool)

-- | Given a group of db tests, build a callback usable by withResource.
-- The sqlbackend pool will need to be threaded to each Assertion (== IO
-- ()) that defines a test.
dbTestGroup :: TestName -> [DBTestTree] -> DBTestTree
dbTestGroup label dbtree mpool = testGroup label (map ($ mpool) dbtree)

withDB :: DBTestTree -> TestTree
withDB = withResource mkTempDBPool cleanseDatabase
  where
    mkTempDBPool = runNoLoggingT $ do
        pgExecute z "create database blablab"
        p <- createPostgresqlPool str 10
        void $ runSqlPool (runMigrationSilent migrateMech) p
        return p
    z = "postgresql:///postgres?"
        <> "host=/home/b/src/Haskell/snowdrift/sd-mechanism/postgres/sockets"
    str = "postgresql:///blablab?"
          <> "host=/home/b/src/Haskell/snowdrift/sd-mechanism/postgres/sockets"
    cleanseDatabase pool = do
        destroyAllResources pool
        void $ pgExecute z "drop database blablab"
    pgExecute constr q = void . liftIO $ bracket (connectPostgreSQL constr)
                                                 close
                                                 (\c -> execute_ c q)

---
tests :: TestTree
tests = withDB $ dbTestGroup "input processor"
    [ dbTestCase "nobody is ever overspent" pending
    , dbTestCase "during payout, projects are independent" pending
    , dbTestGroup "patron must support 3 months of pledging" threeMonths
    ]

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
