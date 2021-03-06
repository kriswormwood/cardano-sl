{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.WalletLayer.Kernel.Wallets (
      createWallet
    , updateWallet
    , updateWalletPassword
    , deleteWallet
    , getWallet
    , getWallets
    , getWalletUtxos
    , blundToResolvedBlock
    ) where

import           Universum

import           Data.Coerce (coerce)
import qualified Data.Map as M
import           Data.Maybe (fromJust)

import           Pos.Chain.Block (Blund, mainBlockSlot, undoTx)
import           Pos.Chain.Txp (Utxo)
import           Pos.Core (Config (..), mkCoin)
import           Pos.Core.Slotting (Timestamp)
import           Pos.Crypto.Signing

import           Cardano.Wallet.API.V1.Types (V1 (..))
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Accounts as Kernel
import qualified Cardano.Wallet.Kernel.BIP39 as BIP39
import           Cardano.Wallet.Kernel.DB.AcidState (dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockContext
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock)
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Internal (walletKeystore,
                     walletRestorationTask)
import qualified Cardano.Wallet.Kernel.Internal as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.NodeStateAdaptor (NodeStateAdaptor)
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock,
                     prefilterBlock)
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Restore (restoreWallet)
import           Cardano.Wallet.Kernel.Types (RawResolvedBlock (..),
                     WalletId (..), fromRawResolvedBlock)
import           Cardano.Wallet.Kernel.Util.Core (getCurrentTimestamp)
import qualified Cardano.Wallet.Kernel.Wallets as Kernel
import           Cardano.Wallet.WalletLayer (CreateWallet (..),
                     CreateWalletError (..), DeleteWalletError (..),
                     GetUtxosError (..), GetWalletError (..),
                     UpdateWalletError (..), UpdateWalletPasswordError (..))
import           Cardano.Wallet.WalletLayer.Kernel.Conv

createWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> CreateWallet
             -> m (Either CreateWalletError V1.Wallet)
createWallet wallet newWalletRequest = liftIO $ do
    now  <- liftIO getCurrentTimestamp
    case newWalletRequest of
        CreateWallet newWallet@V1.NewWallet{..} ->
            case newwalOperation of
                V1.RestoreWallet -> restore newWallet now
                V1.CreateWallet  -> create  newWallet now
        ImportWalletFromESK esk mbSpendingPassword ->
            restoreFromESK esk
                           (spendingPassword mbSpendingPassword)
                           now
                           "Imported Wallet"
                           HD.AssuranceLevelNormal
  where
    create :: V1.NewWallet -> Timestamp -> IO (Either CreateWalletError V1.Wallet)
    create newWallet@V1.NewWallet{..} now = runExceptT $ do
      root <- withExceptT CreateWalletError $ ExceptT $
                Kernel.createHdWallet wallet
                                      (mnemonic newWallet)
                                      (spendingPassword newwalSpendingPassword)
                                      (fromAssuranceLevel newwalAssuranceLevel)
                                      (HD.WalletName newwalName)
      let rootId = root ^. HD.hdRootId
      _ <- withExceptT CreateWalletFirstAccountCreationFailed $ ExceptT $
             Kernel.createAccount (spendingPassword newwalSpendingPassword)
                                  (HD.AccountName "Default account")
                                  (WalletIdHdRnd rootId)
                                  wallet
      return (mkRoot newwalName newwalAssuranceLevel now root)

    restore :: V1.NewWallet
            -> Timestamp
            -> IO (Either CreateWalletError V1.Wallet)
    restore newWallet@V1.NewWallet{..} now = do
        let esk    = snd $ safeDeterministicKeyGen
                             (BIP39.mnemonicToSeed (mnemonic newWallet))
                             (spendingPassword newwalSpendingPassword)
        restoreFromESK esk
                       (spendingPassword newwalSpendingPassword)
                       now
                       newwalName
                       (fromAssuranceLevel newwalAssuranceLevel)

    restoreFromESK :: EncryptedSecretKey
                   -> PassPhrase
                   -> Timestamp
                   -> Text
                   -> HD.AssuranceLevel
                   -> IO (Either CreateWalletError V1.Wallet)
    restoreFromESK esk pwd now walletName hdAssuranceLevel = runExceptT $ do
        let rootId = HD.eskToHdRootId esk
            wId    = WalletIdHdRnd rootId

        -- Insert the 'EncryptedSecretKey' into the 'Keystore'
        liftIO $ Keystore.insert wId esk (wallet ^. walletKeystore)

        -- Synchronously restore the wallet balance, and begin to
        -- asynchronously reconstruct the wallet's history.
        let prefilter :: Blund -> IO (Map HD.HdAccountId PrefilteredBlock, [TxMeta])
            prefilter blund =
                blundToResolvedBlock (wallet ^. Kernel.walletNode) blund <&> \case
                    Nothing -> (M.empty, [])
                    Just rb -> prefilterBlock rb wId esk

        (root, coins) <- withExceptT (CreateWalletError . Kernel.CreateWalletFailed) $ ExceptT $
            restoreWallet
              wallet
              (pwd /= emptyPassphrase)
              (HD.WalletName walletName)
              hdAssuranceLevel
              esk
              prefilter

        -- Return the wallet information, with an updated balance.
        let root' = mkRoot walletName (toAssuranceLevel hdAssuranceLevel) now root
        updateSyncState wallet wId (root' { V1.walBalance = V1 coins })

    mkRoot :: Text -> V1.AssuranceLevel -> Timestamp -> HD.HdRoot -> V1.Wallet
    mkRoot v1WalletName v1AssuranceLevel now hdRoot = V1.Wallet {
          walId                         = walletId
        , walName                       = v1WalletName
        , walBalance                    = V1 (mkCoin 0)
        , walHasSpendingPassword        = hasSpendingPassword
        , walSpendingPasswordLastUpdate = V1 lastUpdate
        , walCreatedAt                  = V1 createdAt
        , walAssuranceLevel             = v1AssuranceLevel
        , walSyncState                  = V1.Synced
        , walType                       = V1.WalletRegular
        }
      where
        (hasSpendingPassword, mbLastUpdate) =
            case hdRoot ^. HD.hdRootHasPassword of
                 HD.NoSpendingPassword     -> (False, Nothing)
                 HD.HasSpendingPassword lu -> (True, Just (lu ^. fromDb))
        lastUpdate = fromMaybe now mbLastUpdate
        createdAt  = hdRoot ^. HD.hdRootCreatedAt . fromDb
        walletId   = toRootId $ hdRoot ^. HD.hdRootId

               -- (V1.BackupPhrase mnemonic)
               -- mbSpendingPassword
               -- v1AssuranceLevel
               -- v1WalletName
               -- operation
    mnemonic (V1.NewWallet (V1.BackupPhrase m) _ _ _ _) = m
    spendingPassword = maybe emptyPassphrase coerce

-- | Updates the 'SpendingPassword' for this wallet.
updateWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> V1.WalletUpdate
             -> m (Either UpdateWalletError V1.Wallet)
updateWallet wallet wId (V1.WalletUpdate v1Level v1Name) = runExceptT $ do
    rootId <- withExceptT UpdateWalletWalletIdDecodingFailed $ fromRootId wId
    v1wal <- fmap (uncurry toWallet) $
               withExceptT (UpdateWalletError . V1) $ ExceptT $ liftIO $
                 Kernel.updateHdWallet wallet rootId newLevel newName
    updateSyncState wallet (WalletIdHdRnd rootId) v1wal
  where
    newLevel = fromAssuranceLevel v1Level
    newName  = HD.WalletName v1Name

-- | Updates the 'SpendingPassword' for this wallet.
updateWalletPassword :: MonadIO m
                     => Kernel.PassiveWallet
                     -> V1.WalletId
                     -> V1.PasswordUpdate
                     -> m (Either UpdateWalletPasswordError V1.Wallet)
updateWalletPassword wallet
                     wId
                     (V1.PasswordUpdate
                       (V1 oldPwd)
                       (V1 newPwd)) = runExceptT $ do
    rootId <- withExceptT UpdateWalletPasswordWalletIdDecodingFailed $
                fromRootId wId
    v1wal <- fmap (uncurry toWallet) $
              withExceptT UpdateWalletPasswordError $ ExceptT $ liftIO $
                Kernel.updatePassword wallet rootId oldPwd newPwd
    updateSyncState wallet (WalletIdHdRnd rootId) v1wal

-- | Updates the 'SpendingPassword' for this wallet.
deleteWallet :: MonadIO m
             => Kernel.PassiveWallet
             -> V1.WalletId
             -> m (Either DeleteWalletError ())
deleteWallet wallet wId = runExceptT $ do
    rootId <- withExceptT DeleteWalletWalletIdDecodingFailed $ fromRootId wId
    withExceptT (DeleteWalletError . V1) $ ExceptT $ liftIO $
      Kernel.deleteHdWallet wallet rootId

-- | Gets a specific wallet.
getWallet :: MonadIO m
          => Kernel.PassiveWallet
          -> V1.WalletId
          -> Kernel.DB
          -> m (Either GetWalletError V1.Wallet)
getWallet wallet wId db = runExceptT $ do
    rootId <- withExceptT GetWalletWalletIdDecodingFailed (fromRootId wId)
    v1wal <- fmap (toWallet db) $
                withExceptT (GetWalletError . V1) $ exceptT $
                    Kernel.lookupHdRootId db rootId
    updateSyncState wallet (WalletIdHdRnd rootId) v1wal

-- | Gets all the wallets known to this edge node.
--
-- NOTE: The wallet sync state is not set here; use 'updateSyncState' to
--       get a correct result.
--
-- TODO: Avoid IxSet creation [CBR-347].
getWallets :: MonadIO m
           => Kernel.PassiveWallet
           -> Kernel.DB
           -> m (IxSet V1.Wallet)
getWallets wallet db =
    fmap IxSet.fromList $ forM (IxSet.toList allRoots) $ \root -> do
        let rootId = root ^. HD.hdRootId
        updateSyncState wallet (WalletIdHdRnd rootId) (toWallet db root)
  where
    allRoots = db ^. dbHdWallets . HD.hdWalletsRoots

-- | Gets Utxos per account of a wallet.
getWalletUtxos
    :: V1.WalletId
    -> Kernel.DB
    -> Either GetUtxosError [(V1.Account, Utxo)]
getWalletUtxos wId db = runExcept $ do
    rootId <- withExceptT GetUtxosWalletIdDecodingFailed $
        fromRootId wId

    withExceptT GetUtxosGetAccountsError $ exceptT $ do
        _rootExists <- Kernel.lookupHdRootId db rootId
        return ()

    let accounts = Kernel.accountsByRootId db rootId

    forM (IxSet.toList accounts) $ \account ->
        withExceptT GetUtxosCurrentAvailableUtxoError $ exceptT $ do
            utxo <- Kernel.currentAvailableUtxo db (account ^. HD.hdAccountId)
            return (toAccount db account, utxo)

-- | The use of the unsafe constructor 'UnsafeRawResolvedBlock' is justified
-- by the invariants established in the 'Blund'.
blundToResolvedBlock :: NodeStateAdaptor IO -> Blund -> IO (Maybe ResolvedBlock)
blundToResolvedBlock node (b,u) = do
    genesisHash <- configGenesisHash <$> Node.getCoreConfig node
    case b of
      Left  _ebb      -> return Nothing
      Right mainBlock -> Node.withNodeState node $ \_lock -> do
        ctxt  <- mainBlockContext genesisHash mainBlock
        mTime <- Node.defaultGetSlotStart (mainBlock ^. mainBlockSlot)
        now   <- liftIO $ getCurrentTimestamp
        return $ Just $ fromRawResolvedBlock UnsafeRawResolvedBlock {
            rawResolvedBlock       = mainBlock
          , rawResolvedBlockInputs = map (map fromJust) $ undoTx u
          , rawTimestamp           = either (const now) identity mTime
          , rawResolvedContext     = ctxt
          }

updateSyncState :: MonadIO m
                => Kernel.PassiveWallet
                -> WalletId
                -> V1.Wallet
                -> m V1.Wallet
updateSyncState wallet wId v1wal = do
    wss <- M.lookup wId <$> readMVar (wallet ^. walletRestorationTask)
    return v1wal { V1.walSyncState = toSyncState wss }
