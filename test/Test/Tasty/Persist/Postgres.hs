{-# LANGUAGE OverloadedStrings #-}

-- | Tools for running standalone tests that use Postgres via Persist. The
-- guiding principle is test independence. The key [tbd] mechanisms are
-- creating temporary databases and rolling back all test-created changes
-- between each test.
module Test.Tasty.Persist.Postgres
        ( -- * getting back to TestTree:s
          withDB
          -- * DBAssertion:s
        , DBAssertion
        , pending
        , shouldBe
          -- * DBTestTree:s
        , DBTestTree
        , dbTestCase
        , dbTestGroup
        ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT(..))
import Data.Monoid ((<>))
import Data.Pool (Pool, destroyAllResources)
import Database.Persist.Postgresql (createPostgresqlPool, PostgresConf(..))
import Database.Persist.Sql
        ( Migration
        , SqlBackend
        , SqlPersistT
        , runSqlPool
        , transactionUndo
        , runMigrationSilent)
import Database.PostgreSQL.Simple (execute_, connectPostgreSQL, close)
import Test.Tasty
import Test.Tasty.HUnit

-- | *Some* kind of MonadLogger is needed to satisfy the types of the
-- library methods used to create a database pool. Since logging isn't
-- actually used yet, I'm using NoLoggingT.
type DBAssertion = SqlPersistT (NoLoggingT IO) ()

-- | A TestTree that depends on a Pool of SqlBackends, for use with
-- 'withResource'
type DBTestTree = IO (Pool SqlBackend) -> TestTree

-- | A stand-in for undefined tests
pending :: MonadIO m => m ()
pending = liftIO (assertFailure "(test is pending)")

-- Convenience for checking database values.
shouldBe :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
ma `shouldBe` b = ma >>= (liftIO . (@?= b))

-- A single database test. Takes place within a transaction that is rolled
-- back at the end of the test. Thus, tests remain independent.
dbTestCase :: TestName -> DBAssertion -> DBTestTree
dbTestCase label stmt mpool = testCase label $ do
    pool <- mpool
    runNoLoggingT (runSqlPool (stmt >> transactionUndo) pool)

-- | Group DBTestTrees. They share a common pool of DB connections.
dbTestGroup :: TestName -> [DBTestTree] -> DBTestTree
dbTestGroup label dbtree mpool = testGroup label (map ($ mpool) dbtree)

-- | Convert a DBTestTree to a TestTree by creating a pool of connections.
--
-- The DBTestTree is bracketed with the creation and destruction of a
-- temporary database. Requires admin privileges. You're using a local db
-- cluster, right?
withDB :: PostgresConf -> Migration -> DBTestTree -> TestTree
withDB conf migrate = withResource mkTempDBPool cleanseDatabase
  where
    mkTempDBPool = runNoLoggingT $ do
        pgExecute z "create database blablab"
        p <- createPostgresqlPool str (pgPoolSize conf)
        void $ runSqlPool (runMigrationSilent migrate) p
        return p
    z = pgConnStr conf <> "&dbname=postgres"
    str = pgConnStr conf <> "&dbname=blablab"
    cleanseDatabase pool = do
        destroyAllResources pool
        void $ pgExecute z "drop database blablab"
    pgExecute constr q = void . liftIO $ bracket (connectPostgreSQL constr)
                                                 close
                                                 (\c -> execute_ c q)
