{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Methods that operate on 'SscGlobalState' and 'VssCertificatesMap'.

module Pos.Ssc.State.Global
       (
       -- * Certs
         getGlobalCerts
       , getStableCerts

       -- * Global state
       , sscLoadGlobalState
       , sscGetGlobalState
       ) where

import           Formatting (build, sformat, (%))
--import           Pos.Util.Log (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Core (EpochIndex (..), HasGenesisData,
                     HasProtocolConstants, SlotId (..),
                     VssCertificatesMap (..))
import           Pos.DB (MonadDBRead)
import qualified Pos.Ssc.DB as DB
import           Pos.Ssc.Functions (getStableCertsPure)
import           Pos.Ssc.Mem (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.Types (SscGlobalState (..), sgsVssCertificates)
import qualified Pos.Ssc.VssCertData as VCD
import           Pos.Util.Trace.Named (TraceNamed, logDebug, logInfo)

----------------------------------------------------------------------------
-- Certs
----------------------------------------------------------------------------

getGlobalCerts
    :: (MonadSscMem ctx m, MonadIO m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        VCD.certs .
        VCD.setLastKnownSlot sl <$>
        view sgsVssCertificates

-- | Get stable VSS certificates for given epoch.
getStableCerts
    :: (MonadSscMem ctx m, MonadIO m, HasGenesisData, HasProtocolConstants)
    => EpochIndex -> m VssCertificatesMap
getStableCerts epoch =
    getStableCertsPure epoch <$> sscRunGlobalQuery (view sgsVssCertificates)

----------------------------------------------------------------------------
-- Seed
----------------------------------------------------------------------------

-- | Load global state from DB by recreating it from recent blocks.
sscLoadGlobalState
    :: (MonadDBRead m)
    => TraceNamed m
    -> m SscGlobalState
sscLoadGlobalState logTrace = do
    logDebug logTrace "Loading SSC global state"
    gs <- DB.getSscGlobalState
    gs <$ logInfo logTrace (sformat ("Loaded SSC state: " %build) gs)

sscGetGlobalState
    :: (MonadSscMem ctx m, MonadIO m)
    => m SscGlobalState
sscGetGlobalState = sscRunGlobalQuery ask