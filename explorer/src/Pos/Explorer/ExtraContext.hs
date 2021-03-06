-- Classes and datatypes for managing extra context in Explorer.

{-# LANGUAGE RankNTypes #-}

module Pos.Explorer.ExtraContext
    ( ExtraContext (..)
    , ExtraContextT
    , runExtraContextT
    , makeExtraCtx

    , HasGenesisRedeemAddressInfo (..)
    , GenesisRedeemAddressInfo
    -- * Genesis address info

    , HasExplorerCSLInterface (..)
    , ExplorerMockableMode (..)
    , makeMockExtraCtx
    -- * Explorer mock interface

    ) where

import           Universum

import qualified Data.Vector as V
import qualified Ether

import           Data.Default (Default (..), def)
import           Pos.Chain.Block (Block, Blund, HeaderHash)
import qualified Pos.DB.Block as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.Explorer.DB (Epoch, Page, getEpochBlocks, getEpochPages,
                     getPageBlocks)

import           Pos.Chain.Txp (genesisUtxo, utxoToAddressCoinPairs)
import           Pos.Core as Core (Address, Coin, Config (..), EpochIndex,
                     GenesisHash, SlotId (..), SlotLeaders, Timestamp,
                     isRedeemAddress)
import           Pos.DB.Lrc (getLeadersForEpoch)
import           Pos.Infra.Slotting (MonadSlotsData, getSlotStart)


-------------------------------------------------------------------------------------
-- Extra context
-------------------------------------------------------------------------------------

type ExtraContextT m = Ether.ReaderT' ExtraContext m

runExtraContextT :: ExtraContext -> ExtraContextT m a -> m a
runExtraContextT = flip Ether.runReaderT

data ExtraContext = ExtraContext
    { ecAddressCoinPairs     :: !GenesisRedeemAddressInfo
    , ecExplorerMockableMode :: !ExplorerMockableMode
    }

makeExtraCtx :: Core.Config -> ExtraContext
makeExtraCtx coreConfig =
    let addressCoinPairs =
            utxoToAddressCoinPairs $ genesisUtxo $ configGenesisData coreConfig
        redeemOnly = filter (isRedeemAddress . fst) addressCoinPairs
    in ExtraContext
        { ecAddressCoinPairs     = V.fromList redeemOnly
        , ecExplorerMockableMode = prodMode $ configGenesisHash coreConfig
        }

-- | For mocking we mostly need to replace just the external CSL functions.
makeMockExtraCtx :: ExplorerMockableMode -> ExtraContext
makeMockExtraCtx explorerMockMode =
    ExtraContext
        { ecAddressCoinPairs = V.empty
        , ecExplorerMockableMode = explorerMockMode
        }

-------------------------------------------------------------------------------------
-- Genesis redeem address info
-------------------------------------------------------------------------------------

type GenesisRedeemAddressInfo = V.Vector (Address, Coin)

class HasGenesisRedeemAddressInfo m where
    getGenesisRedeemAddressInfo :: m GenesisRedeemAddressInfo

instance Monad m => HasGenesisRedeemAddressInfo (ExtraContextT m) where
    getGenesisRedeemAddressInfo = do
        extraCtx <- Ether.ask @ExtraContext
        pure $ ecAddressCoinPairs extraCtx

-------------------------------------------------------------------------------------
-- Explorer mock mode
--
-- The simple data structure that encapsulates functions that use CSL. We want to "cut"
-- them out of the picture in order to be able to mock them.
-------------------------------------------------------------------------------------

data ExplorerMockableMode = ExplorerMockableMode
    { emmGetTipBlock
          :: forall m. MonadDBRead m => m Block
    , emmGetPageBlocks
          :: forall m. MonadDBRead m => Page -> m (Maybe [HeaderHash])
    , emmGetBlundFromHH
          :: forall m. MonadDBRead m => HeaderHash -> m (Maybe Blund)
    , emmGetSlotStart
          :: forall ctx m. MonadSlotsData ctx m => SlotId -> m (Maybe Timestamp)
    , emmGetLeadersFromEpoch
          :: forall m. MonadDBRead m => EpochIndex -> m (Maybe SlotLeaders)
    , emmGetEpochBlocks
          :: forall m. MonadDBRead m => Epoch -> Page -> m (Maybe [HeaderHash])
    , emmGetEpochPages
          :: forall m. MonadDBRead m => Epoch -> m (Maybe Page)
    }

-- | This is what we use in production when we run Explorer.
prodMode :: GenesisHash -> ExplorerMockableMode
prodMode genesisHash = ExplorerMockableMode {
      emmGetTipBlock            = DB.getTipBlock genesisHash,
      emmGetPageBlocks          = getPageBlocks,
      emmGetBlundFromHH         = DB.getBlund genesisHash,
      emmGetSlotStart           = getSlotStart,
      emmGetLeadersFromEpoch    = getLeadersForEpoch,
      emmGetEpochBlocks         = getEpochBlocks,
      emmGetEpochPages          = getEpochPages
    }

-- | So we can just reuse the default instance and change individial functions.
-- On one side, it removes the compile error(s) for having all functions implemented.
-- On the other side, it moves that error into runtime and enables simple mocking.
-- This is a good thing once we have a larger amount of functions, like in _explorer_,
-- and this gives us the flexibility to "mock" whichever we want.
instance Default (ExplorerMockableMode) where
  def = ExplorerMockableMode {
        emmGetTipBlock            = errorImpl,
        emmGetPageBlocks          = errorImpl,
        emmGetBlundFromHH         = errorImpl,
        emmGetSlotStart           = errorImpl,
        emmGetLeadersFromEpoch    = errorImpl,
        emmGetEpochBlocks         = errorImpl,
        emmGetEpochPages          = errorImpl
      }
    where
      errorImpl = error "Cannot be used, please implement this function!"


-------------------------------------------------------------------------------------
-- Explorer interface instance
-------------------------------------------------------------------------------------

-- | We use this for an external CSL functions representation so we can mock them when
-- testing.
class HasExplorerCSLInterface m where
    getTipBlockCSLI :: m Block
    getPageBlocksCSLI :: Page -> m (Maybe [HeaderHash])
    getBlundFromHHCSLI :: HeaderHash -> m (Maybe Blund)
    getSlotStartCSLI :: SlotId -> m (Maybe Timestamp)
    getLeadersFromEpochCSLI :: EpochIndex -> m (Maybe SlotLeaders)
    getEpochBlocksCSLI :: Epoch -> Page -> m (Maybe [HeaderHash])
    getEpochPagesCSLI :: Epoch -> m (Maybe Page)

-- | The instance for external CSL functions.
instance (Monad m, MonadDBRead m, MonadSlotsData ctx m) =>
    HasExplorerCSLInterface (ExtraContextT m) where

    getTipBlockCSLI = do
        extraCtx <- Ether.ask @ExtraContext
        let explorerMockMode = ecExplorerMockableMode extraCtx
        emmGetTipBlock explorerMockMode

    getPageBlocksCSLI page = do
        extraCtx <- Ether.ask @ExtraContext
        let explorerMockMode = ecExplorerMockableMode extraCtx
        emmGetPageBlocks explorerMockMode page

    getBlundFromHHCSLI headerHash = do
        extraCtx <- Ether.ask @ExtraContext
        let explorerMockMode = ecExplorerMockableMode extraCtx
        emmGetBlundFromHH explorerMockMode headerHash

    getSlotStartCSLI slotId = do
        extraCtx <- Ether.ask @ExtraContext
        let explorerMockMode = ecExplorerMockableMode extraCtx
        emmGetSlotStart explorerMockMode slotId

    getLeadersFromEpochCSLI epochIndex = do
        extraCtx <- Ether.ask @ExtraContext
        let explorerMockMode = ecExplorerMockableMode extraCtx
        emmGetLeadersFromEpoch explorerMockMode epochIndex

    getEpochBlocksCSLI epoch page = do
        extraCtx <- Ether.ask @ExtraContext
        let explorerMockMode = ecExplorerMockableMode extraCtx
        emmGetEpochBlocks explorerMockMode epoch page

    getEpochPagesCSLI epoch = do
        extraCtx <- Ether.ask @ExtraContext
        let explorerMockMode = ecExplorerMockableMode extraCtx
        emmGetEpochPages explorerMockMode epoch
