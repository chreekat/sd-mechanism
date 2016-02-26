{-# LANGUAGE OverloadedStrings #-}

-- | Tools for running standalone tests that use Postgres via Persist. The
-- guiding principle is test independence. The key mechanisms are creating
-- temporary databases and rolling back all test-created changes between
-- each test.
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
import Data.ByteString.Char8 (pack)
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
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

-- | *Some* kind of MonadLogger is needed to satisfy the types of the
-- library methods used to create a database pool. Since logging isn't
-- actually used yet, I'm using NoLoggingT.
type DBAssertion = SqlPersistT (NoLoggingT IO) ()

data TmpDB = TmpDB
        { tmpPool :: Pool SqlBackend
        , tmpName :: String
        }

-- | A TestTree that depends on a Pool of SqlBackends, for use with
-- 'withResource'
type DBTestTree = IO TmpDB -> TestTree

-- | A stand-in for undefined tests
pending :: MonadIO m => m ()
pending = liftIO (assertFailure "(test is pending)")

-- Convenience for checking database values.
shouldBe :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
ma `shouldBe` b = ma >>= (liftIO . (@?= b))

-- A single database test. Takes place within a transaction that is rolled
-- back at the end of the test. Thus, tests remain independent.
dbTestCase :: TestName -> DBAssertion -> DBTestTree
dbTestCase label stmt tmpdb = testCase label $ do
    pool <- tmpPool <$> tmpdb
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
        tmpname <- liftIO mkTempName
        pgExecute z (createQuery tmpname)
        p <- createPostgresqlPool (connect tmpname) (pgPoolSize conf)
        void $ runSqlPool (runMigrationSilent migrate) p
        return (TmpDB p tmpname)
    z = pgConnStr conf <> "&dbname=postgres"
    connect name = pgConnStr conf <> "&dbname=" <> pack name
    cleanseDatabase tmpDB = do
        destroyAllResources (tmpPool tmpDB)
        void $ pgExecute z (dropQuery (tmpName tmpDB))
    pgExecute connstr q = void . liftIO $ bracket (connectPostgreSQL connstr)
                                                  close
                                                  (`execute_` q)
    -- YELLOW ALERT: These names and queries are artisanally crafted to
    -- avoid injection attacks and syntax errors.
    mkTempName = ("tasty_" <>)
               . take 12
               . randomRs ('a', 'z')
               <$> getStdGen
    -- ...this is hilarious, though. There's no real way to do this.
    -- Attempting to use query paramaters ("create database ?") causes the
    -- db name to be wrapped in single-quote chars, which is a syntax
    -- error.
    createQuery db = "create database "
                   <> read ("\"" <> db <> "\"")
    dropQuery db = "drop database "
                   <> read ("\"" <> db <> "\"")
