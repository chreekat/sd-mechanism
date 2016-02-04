-- For type Mech m a
{-# LANGUAGE Rank2Types #-}
module SDMechanism where

import Control.Error
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Data.Maybe (fromMaybe)
import Database.Persist
import Database.Persist.Sql (SqlBackend)

import Types
import Persist

type Mech m a = MonadIO m => ReaderT SqlBackend m a
type EMech a = MonadIO m => ExceptT MechError (ReaderT SqlBackend m) a

-- | Convert user's value to a MechPatron
importPatron :: ToMechPatron a => a -> EMech (Entity MechPatron)
importPatron = (!? NoSuchPatron) . getBy . ExternalPatron . mechPatron

-- | Convert user's value to a MechProjectId
importProject :: ToMechProject a => a -> EMech (Entity MechProject)
importProject = (!? NoSuchProject) . getBy . ExternalProject . mechProject

right :: Monad m => m a -> ExceptT e m a
right = ExceptT . fmap Right

newPledge :: (ToMechPatron pat, ToMechProject pro)
          => pro -> pat -> Mech m (Either MechError ())
newPledge r a = runExceptT $ do
    pro <- importProject r
    pat <- importPatron a
    hoistEither (Right ())

-- deletePledge :: (ToMechPatron pat, ToMechProject pro)
--              => pro-> pat -> Mech m ()
-- deletePledge r a = exceptT return return $ do
--     let pro = importProject r
--         pat = importPatron a
--     right (deleteBy (UniquePledge pro pat))

-- tellPledges :: (ToMechProject pro, ToMechPatron pat) => pro -> Mech m [pat]
-- tellPledges a = fmap (map export) (selectList [PledgeProject ==. (ma a)] [])
--   where export = toPatron . pledgePatron . entityVal
--
availableFunds = undefined
payout = undefined



    -- PayoutAll -> 
    -- PayoutOne pat
    -- TellPayoutHistory Int pat

