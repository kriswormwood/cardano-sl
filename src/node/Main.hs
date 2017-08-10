{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum           hiding (over)

import           Control.Lens        (views)
import           Data.Maybe          (fromJust)
import           Ether.Internal      (HasLens (..))
import           Formatting          (sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)
import           System.Wlog         (logError, logInfo)

import           Pos.Binary          ()
import qualified Pos.CLI             as CLI
import           Pos.Communication   (ActionSpec (..), OutSpecs, WorkerSpec, worker)
import           Pos.Constants       (isDevelopment)
import           Pos.Context         (HasNodeContext)
import           Pos.Core            (HasCoreConstants, Timestamp (..), giveStaticConsts)
import           Pos.Launcher        (NodeParams (..), runNodeReal)
import           Pos.Security        (SecurityWorkersClass)
import           Pos.Shutdown        (triggerShutdown)
import           Pos.Ssc.Class       (SscConstraint, SscParams)
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Ssc.NistBeacon  (SscNistBeacon)
import           Pos.Ssc.SscAlgo     (SscAlgo (..))
import           Pos.Update.Context  (UpdateContext, ucUpdateSemaphore)
import           Pos.Util            (inAssertMode)
import           Pos.Util.UserSecret (usVss)
import           Pos.WorkMode        (RealMode, WorkMode)
#ifdef WITH_WEB
import           Pos.Web             (serveWebGT)
#ifdef WITH_WALLET
import           Pos.Wallet.Web      (WalletWebMode, runWalletNodeReal,
                                      walletServeWebFull, walletServerOuts)
#endif
#endif

import           NodeOptions         (Args (..), getNodeOptions)
import           Params              (getNodeParams, gtSscParams)

----------------------------------------------------------------------------
-- Without wallet
----------------------------------------------------------------------------

actionWithoutWallet ::
       forall ssc. (SscConstraint ssc, SecurityWorkersClass ssc)
    => SscParams ssc
    -> NodeParams
    -> Production ()
actionWithoutWallet sscParams nodeParams =
    runNodeReal @ssc nodeParams sscParams plugins
  where
    plugins :: ([WorkerSpec (RealMode ssc)], OutSpecs)
    plugins = updateTriggerWorker

updateTriggerWorker
    :: SscConstraint ssc
    => ([WorkerSpec (RealMode ssc)], OutSpecs)
updateTriggerWorker = first pure $ worker mempty $ \_ -> do
    logInfo "Update trigger worker is locked"
    void $ takeMVar =<< views (lensOf @UpdateContext) ucUpdateSemaphore
    triggerShutdown

----------------------------------------------------------------------------
-- With wallet
----------------------------------------------------------------------------

#ifdef WITH_WALLET

actionWithWallet :: SscParams SscGodTossing -> NodeParams -> Args -> Production ()
actionWithWallet sscParams nodeParams args@Args {..} =
    runWalletNodeReal walletDbPath walletRebuildDb sscParams nodeParams plugins
  where
    convPlugins = (, mempty) . map (\act -> ActionSpec $ \__vI __sA -> act)
    plugins :: HasCoreConstants => ([WorkerSpec WalletWebMode], OutSpecs)
    plugins = convPlugins (pluginsGT args) <> walletProd args

walletProd :: HasCoreConstants => Args -> ([WorkerSpec WalletWebMode], OutSpecs)
walletProd Args {..} = first pure $ worker walletServerOuts $ \sendActions ->
    walletServeWebFull
        sendActions
        walletDebug
        walletPort
        (Just walletTLSParams)

#else

actionWithWallet :: panic
actionWithWallet = error "actionWithWallet"

#endif

#ifdef WITH_WEB
pluginsGT ::
    ( WorkMode SscGodTossing ctx m
    , HasNodeContext SscGodTossing ctx
    ) => Args -> [m ()]
pluginsGT Args {..}
    | enableWeb = [serveWebGT webPort (Just walletTLSParams)]
    | otherwise = []
#endif

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

printFlags :: IO ()
printFlags = do
    if isDevelopment
        then putText "[Attention] We are in DEV mode"
        else putText "[Attention] We are in PRODUCTION mode"
#ifdef WITH_WEB
    putText "[Attention] Software is built with web part"
#endif
#ifdef WITH_WALLET
    putText "[Attention] Software is built with wallet part"
#endif
    inAssertMode $ putText "Asserts are ON"

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getNodeOptions
    printFlags
    runProduction (action args)

action :: Args -> Production ()
action args@Args {..} = do
    systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- getNodeParams args systemStart
    putText $ "Running using " <> show (CLI.sscAlgo commonArgs)
#ifdef WITH_WALLET
    putText $ "Is wallet enabled: " <> show enableWallet
#else
    putText "Wallet is disabled, because software is built w/o it"
#endif

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = gtSscParams args vssSK

    let sscParams :: Either (SscParams SscNistBeacon) (SscParams SscGodTossing)
        sscParams = bool (Left ()) (Right gtParams) (CLI.sscAlgo commonArgs == GodTossingAlgo)
#if defined(WITH_WALLET) && defined(WITH_WEB)
    let userWantsWallet = enableWallet
#else
    let userWantsWallet = False
#endif
    case (userWantsWallet, sscParams) of
        (False, Left par)  -> actionWithoutWallet @SscNistBeacon par currentParams
        (False, Right par) ->
            -- TODO: What the heck?!
            -- @actionWithoutWallet@ requires HasCoreConstants, why?
            giveStaticConsts (actionWithoutWallet @SscGodTossing par currentParams)
        (True, Left _)     -> logError "Wallet does not support NIST beacon!"
        (True, Right par)  -> actionWithWallet par currentParams args
