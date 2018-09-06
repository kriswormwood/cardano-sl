{- | A collection of plugins used by this edge node.
     A @Plugin@ is essentially a set of actions which will be run in
     a particular monad, at some point in time.
-}

module Cardano.Wallet.Server.Plugins
    ( Plugin
    , apiServer
    , docServer
    , monitoringServer
    , acidStateSnapshots
    , updateNotifier
    ) where

import           Universum

import qualified Data.ByteString.Char8 as BS8
import           Data.Aeson
import           Network.HTTP.Types.Status as Http
import           Network.Wai (Application, Middleware, responseLBS)
import           Network.Wai.Handler.Warp (defaultSettings)
import           Network.Wai.Middleware.Cors (cors, corsMethods,
                     corsRequestHeaders, simpleCorsResourcePolicy,
                     simpleMethods)
import qualified Network.Wai.Middleware.Throttle as Throttle
import qualified Servant
import           Pos.Util.Wlog (logError, logInfo, usingLoggerName)

import           Cardano.NodeIPC (startNodeJsIPC)
import           Cardano.Wallet.API as API
import qualified Cardano.Wallet.API.V1.Types as V1
import           Cardano.Wallet.Kernel (PassiveWallet)
import qualified Cardano.Wallet.Kernel.Diffusion as Kernel
import qualified Cardano.Wallet.Kernel.Mode as Kernel
import qualified Cardano.Wallet.Server as Server
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     RunMode, WalletBackendParams (..), isDebugMode,
                     walletDbOptions)
import           Cardano.Wallet.WalletLayer (ActiveWalletLayer,
                     PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel
import           Pos.Web (serveDocImpl, serveImpl)

import           Pos.Crypto (ProtocolMagic)
import           Pos.Infra.Diffusion.Types (Diffusion (..))
import           Pos.Infra.Shutdown (HasShutdownContext (shutdownContext),
                      ShutdownContext)
import           Pos.Launcher.Configuration (HasConfigurations,
                     ThrottleSettings (..), WalletConfiguration (..))
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Wallet.Web () -- Needed for Orphan Instance 'Buildable Servant.NoContent' :|
import qualified Pos.Web.Server

-- A @Plugin@ running in the monad @m@.
type Plugin m = [Diffusion m -> m ()]

-- | A @Plugin@ to start the wallet REST server
apiServer
    :: ProtocolMagic
    -> WalletConfiguration
    -> NewWalletBackendParams
    -> (PassiveWalletLayer IO, PassiveWallet)
    -> Plugin Kernel.WalletMode
apiServer protocolMagic walletConfiguration (NewWalletBackendParams WalletBackendParams{..}) (passiveLayer, passiveWallet) =
    pure $ \diffusion -> do
        env <- ask
        let diffusion' = Kernel.fromDiffusion (lower env) diffusion
        WalletLayer.Kernel.bracketActiveWallet protocolMagic passiveLayer passiveWallet diffusion' $ \active _ -> do
          ctx <- view shutdownContext
          serveImpl
            (getApplication active)
            (BS8.unpack ip)
            port
            (if isDebugMode walletRunMode then Nothing else walletTLSParams)
            Nothing
            (Just $ portCallback ctx)
  where
    (ip, port) = walletAddress

    getApplication :: ActiveWalletLayer IO -> Kernel.WalletMode Application
    getApplication active = do
        logInfo "New wallet API has STARTED!"
        withMiddleware walletConfiguration walletRunMode $
            Servant.serve API.newWalletAPI $
                Server.walletServer active walletRunMode

    lower :: env -> ReaderT env IO a -> IO a
    lower env m = runReaderT m env

    portCallback :: ShutdownContext -> Word16 -> IO ()
    portCallback ctx =
        usingLoggerName "NodeIPC" . flip runReaderT ctx . startNodeJsIPC

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = "PUT" : simpleMethods
        }


-- | A @Plugin@ to serve the wallet documentation
docServer
    :: (HasConfigurations, HasCompileInfo)
    => NewWalletBackendParams
    -> Plugin Kernel.WalletMode
docServer (NewWalletBackendParams WalletBackendParams{..}) = pure $ \_ ->
    serveDocImpl
        application
        (BS8.unpack ip)
        port
        (if isDebugMode walletRunMode then Nothing else walletTLSParams)
        (Just defaultSettings)
        Nothing
  where
    (ip, port) = walletDocAddress

    application :: Kernel.WalletMode Application
    application =
        return $ Servant.serve API.newWalletDocAPI Server.walletDocServer

-- | A @Plugin@ to serve the node monitoring API.
monitoringServer :: HasConfigurations
                 => NewWalletBackendParams
                 -> Plugin Kernel.WalletMode
monitoringServer (NewWalletBackendParams WalletBackendParams{..}) =
    case enableMonitoringApi of
         True  -> pure $ \_ -> do
             serveImpl Pos.Web.Server.application
                       "127.0.0.1"
                       monitoringApiPort
                       walletTLSParams
                       Nothing
                       Nothing
         False -> []

-- | "Attaches" the middleware to this 'Application', if any.  When running in
-- debug mode, chances are we want to at least allow CORS to test the API with
-- a Swagger editor, locally.
withMiddleware
    :: MonadIO m
    => WalletConfiguration
    -> RunMode
    -> Application
    -> m Application
withMiddleware walletConfiguration wrm app = do
    throttling <- case ccThrottle walletConfiguration of
        Nothing ->
            pure identity
        Just ts -> do
            throttler <- liftIO $ Throttle.initThrottler
            pure (Throttle.throttle (throttleSettings ts) throttler)
    pure
        . (if isDebugMode wrm then corsMiddleware else identity)
        . throttling
        $ app
  where
    throttleSettings ts = Throttle.defaultThrottleSettings
        { Throttle.onThrottled = \microsTilRetry ->
            responseLBS
                Http.status429
                [ ("Content-Type", "application/json") ]
                (encode (V1.RequestThrottled microsTilRetry))
        , Throttle.throttleRate = fromIntegral $ tsRate ts
        , Throttle.throttlePeriod = fromIntegral $ tsPeriod ts
        , Throttle.throttleBurst = fromIntegral $ tsBurst ts
        }

-- | A @Plugin@ to periodically compact & snapshot the acid-state database.
acidStateSnapshots :: Plugin Kernel.WalletMode
acidStateSnapshots = [
    \_diffusion -> logError "Not Implemented: acidStateSnapshots [CBR-305]"
    ]

-- | A @Plugin@ to notify frontend via websockets.
updateNotifier :: Plugin Kernel.WalletMode
updateNotifier = [
    \_diffusion -> logError "Not Implemented: updateNotifier [CBR-374]"
    ]
