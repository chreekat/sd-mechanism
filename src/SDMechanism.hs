-- For type Mech m a
{-# LANGUAGE Rank2Types #-}
module SDMechanism where

import Control.Monad (void, forM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT)
import Data.Maybe (fromMaybe)
import Database.Persist
import Database.Persist.Sql (SqlBackend)

import Types
import Persist

type Mech m a = MonadIO m => ReaderT SqlBackend m a

newPledge :: (ToMechProject pro, ToMechPatron pat)
          => pat-> pro -> Mech m PledgeResult
newPledge a r = do
    existingPledge <- getBy (UniquePledge pat pro)
    res <- forM existingPledge $ \_ -> do
        funds <- availableFunds pat
        ct <- patronCount pro
        let outlay = payout (ct + 1)
        case compare funds (3 * outlay) of
            LT -> return InsufficientFunds
            GT -> return NewPledge <* upsert (Pledge pat pro) []
    return (fromMaybe ExistingPledge res)
  where pat = mechPatron a
        pro = mechProject r

availableFunds = undefined
patronCount = undefined
payout = undefined

data PledgeResult = InsufficientFunds | NewPledge | ExistingPledge

-- deletePledge :: (ToMechProject pro, ToMechPatron pat)
--              => pat-> pro -> Mech m ()
-- deletePledge a r = void (deleteBy (UniquePledge (ma a) (mr r)))

-- tellPledges :: (ToMechPatron pat, ToMechProject pro) => pat -> Mech m [pro]
-- tellPledges a = fmap (map export) (selectList [PledgePatron ==. (ma a)] [])
--   where export = toProject . pledgeProject . entityVal

    -- PayoutAll -> 
    -- PayoutOne pro
    -- TellPayoutHistory Int pro

