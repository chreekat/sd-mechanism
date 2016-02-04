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

importPatron a = (hoistEither . fmapL BadKey . keyFromValues) [PersistInt64 (mechPatron a)]
importProject r = (hoistEither . fmapL BadKey . keyFromValues) [PersistInt64 (mechProject r)]

right :: Monad m => m a -> ExceptT e m a
right = ExceptT . fmap Right

bail = \case
    IMechError e' -> (error . ("Mechanism: " ++) . ibail') e'
    e' -> return e'

newPledge :: (ToMechPatron pat, ToMechProject pro)
          => pro -> pat -> Mech m ()
newPledge r a = exceptT bail return $ do
    pro <- importProject r
    pat <- importPatron a
    existingPledge <- getBy (UniquePledge pro pat) !? ExistingPledge
    funds <- right (availableFunds pro)
    ct <- right (count [PledgePatron ==. pat])
    let outlay = payout (ct + 1)
    case compare funds (3 * outlay) of
        LT -> throwE InsufficientFunds
        _  -> right (insert_ (Pledge pro pat))

deletePledge :: (ToMechPatron pat, ToMechProject pro)
             => pro-> pat -> Mech m ()
deletePledge r a = exceptT return return $ do
    pro <- importProject r
    pat <- importPatron a
    right (deleteBy (UniquePledge pro pat))

-- tellPledges :: (ToMechProject pro, ToMechPatron pat) => pro -> Mech m [pat]
-- tellPledges a = fmap (map export) (selectList [PledgeProject ==. (ma a)] [])
--   where export = toPatron . pledgePatron . entityVal
--
availableFunds = undefined
payout = undefined



    -- PayoutAll -> 
    -- PayoutOne pat
    -- TellPayoutHistory Int pat

