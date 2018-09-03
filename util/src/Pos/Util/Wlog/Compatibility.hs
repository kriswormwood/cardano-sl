module Pos.Util.Wlog.Compatibility
        (  -- * CanLog
           CanLog (..)
         , WithLogger
           -- * Pure logging
         , dispatchEvents
         , LogEvent (..)
         , setupLogging
           -- * Logging functions
         , logDebug
         , logError
         , logInfo
         , logNotice
         , logWarning
         , logMessage
         , LoggerName
         , LoggerNameBox (..)
         , HasLoggerName (..)
         , usingLoggerName
         , LoggerConfig (..)
         , Severity (..)

         , loggingHandler
         , test
         ) where

import           Control.Lens (each)
import qualified Katip as K
import           Pos.Util.Log (LoggerConfig (..), Severity (..))
import qualified Pos.Util.Log as Log
import qualified Pos.Util.Log.Internal as Internal
import           Pos.Util.Log.LoggerConfig (BackendKind (..),
                     RotationParameters (..), defaultInteractiveConfiguration,
                     lcBasePath, lcLoggerTree, lcRotation, lhBackend, lhFpath,
                     lhMinSeverity, lhName, ltHandlers)
import           Pos.Util.Log.Scribes (mkDevNullScribe, mkJsonFileScribe,
                     mkStderrScribe, mkStdoutScribe, mkTextFileScribe)
import           System.IO.Unsafe (unsafePerformIO)

import           Universum

import           Control.Monad.Morph (MFunctor (..))
import qualified Control.Monad.State.Lazy as StateLazy
import           Data.Sequence ((|>))
import           Data.Text (Text)

type LoggerName = Text

-- setupLogging will setup global shared logging state (MVar)
-- usingLoggerName launches an action (of 'LoggerNameBox m a' ~ 'ReaderT LoggerName m a')
-- LoggerNameBox is an instance of CanLog, HasLoggerName
-- our functions are running in this context: 'type WithLogger m = (CanLog m, HasLoggerName m)'

-- this fails with 'MonadIO' redundant constraint
-- type WithLogger m = (CanLog m, HasLoggerName m, Log.LogContext m)

{-
class GHC.Base.Monad m => CanLog (m :: * -> *) where
  dispatchMessage :: LoggerName
                     -> Severity -> Data.Text.Internal.Text -> m ()
  default dispatchMessage :: (Control.Monad.Trans.Class.MonadTrans t,
                              t n ~ m, CanLog n) =>
                             LoggerName -> Severity -> Data.Text.Internal.Text -> m ()
  -- Defined in ‘System.Wlog.CanLog’
instance GHC.Base.Monad m => CanLog (NamedPureLogger m)
  -- Defined in ‘System.Wlog.PureLogging’
instance CanLog m => CanLog (LoggerNameBox m)
  -- Defined in ‘System.Wlog.CanLog’
-}
class Monad m => CanLog m where
    dispatchMessage :: LoggerName -> Severity -> Text -> m ()

    default dispatchMessage :: (MonadTrans t, t n ~ m, CanLog n)
                            => LoggerName
                            -> Severity
                            -> Text
                            -> m ()
    dispatchMessage name severity msg = lift $ dispatchMessage name severity msg

instance CanLog m => CanLog (LoggerNameBox m)
instance CanLog m => CanLog (ReaderT r m)
instance CanLog m => CanLog (StateT s m)
instance CanLog m => CanLog (StateLazy.StateT s m)
instance CanLog m => CanLog (ExceptT s m)

type WithLogger m = (CanLog m, HasLoggerName m)

logDebug, logInfo, logNotice, logWarning, logError
  :: WithLogger m
  => Text -> m ()
logDebug   = logMessage Debug
logInfo    = logMessage Info
logNotice  = logMessage Notice
logWarning = logMessage Warning
logError   = logMessage Error

logMessage :: WithLogger m => Severity -> Text -> m ()
logMessage severity msg = do
    name <- askLoggerName
    dispatchMessage name severity msg

newtype LoggerNameBox m a = LoggerNameBox
    {loggerNameBoxEntry :: ReaderT LoggerName m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans,
    MonadThrow, MonadCatch, MonadMask, MonadState s)

instance MonadReader r m => MonadReader r (LoggerNameBox m) where
    ask = lift ask
    reader = lift . reader
    local f (LoggerNameBox m) = askLoggerName >>= lift . local f . runReaderT m

instance MFunctor LoggerNameBox where
    hoist f = LoggerNameBox . hoist f . loggerNameBoxEntry

usingLoggerName :: LoggerName -> LoggerNameBox m a -> m a
usingLoggerName name = flip runReaderT name . loggerNameBoxEntry

-- HasLoggerName
class HasLoggerName m where

  askLoggerName :: m LoggerName
  modifyLoggerName :: (LoggerName -> LoggerName) -> m a -> m a

  default askLoggerName :: (MonadTrans t, t n ~ m, Monad n, HasLoggerName n)
                        => m LoggerName
  askLoggerName = lift askLoggerName

  default modifyLoggerName :: (MFunctor t, t n ~ m, Monad n, HasLoggerName n)
                           => (LoggerName -> LoggerName) -> m a -> m a
  modifyLoggerName f = hoist (modifyLoggerName f)

  -- Defined in ‘System.Wlog.HasLoggerName’

-- instance Monad m => HasLoggerName (NamedPureLogger m)
--   -- Defined in ‘System.Wlog.PureLogging’
instance (Monad m, HasLoggerName m) => HasLoggerName (StateT a m)
  -- Defined in ‘System.Wlog.HasLoggerName’
instance (Monad m, HasLoggerName m) => HasLoggerName (ReaderT a m)
  -- Defined in ‘System.Wlog.HasLoggerName’
instance HasLoggerName Identity where
    askLoggerName    = Identity "Identity"
    modifyLoggerName = flip const
  -- Defined in ‘System.Wlog.HasLoggerName’
instance (Monad m, HasLoggerName m) => HasLoggerName (ExceptT e m)
  -- Defined in ‘System.Wlog.HasLoggerName’
instance Monad m => HasLoggerName (LoggerNameBox m) where
  askLoggerName = LoggerNameBox ask
  modifyLoggerName how = LoggerNameBox . local how . loggerNameBoxEntry

{-
launchNamedPureLog ::
  (System.Wlog.CanLog.WithLogger n, Monad m) =>
  (forall (f :: * -> *). Functor f => m (f a) -> n (f b))
  -> NamedPureLogger m a -> n b
  	-- Defined in ‘System.Wlog.PureLogging’
-}
-- launchNamedPureLog
--     :: (WithLogger n, Monad m)
--     => (forall f. Functor f => m (f a) -> n (f b))
--     -> NamedPureLogger m a
--     -> n b
-- launchNamedPureLog hoist' namedPureLogger = do
--     name <- askLoggerName
--     (logs, res) <- hoist' $ swap <$> usingNamedPureLogger name namedPureLogger
--     res <$ dispatchEvents logs

-- usingNamedPureLogger :: Functor m
--                      => LoggerName
--                      -> NamedPureLogger m a
--                      -> m (a, [LogEvent])
-- usingNamedPureLogger name (NamedPureLogger action) =
--     usingLoggerName name $ runPureLog action

data LogEvent = LogEvent
    { leLoggerName :: !LoggerName
    , leSeverity   :: !Severity
    , leMessage    :: !Text
    } deriving (Show)

-- runPureLog :: Functor m => PureLogger m a -> m (a, [LogEvent])
-- runPureLog = fmap (second toList) . usingStateT mempty . runPureLogger

-- |
newtype PureLogger m a = PureLogger
    { runPureLogger :: StateT (Seq LogEvent) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadState (Seq LogEvent),
                MonadThrow, HasLoggerName)

instance Monad m => CanLog (PureLogger m) where
    dispatchMessage name severity message = modify' (|> (LogEvent name severity message))
instance MFunctor PureLogger where
    hoist f = PureLogger . hoist f . runPureLogger

-- |
-- newtype NamedPureLogger m a = NamedPureLogger
--     { runNamedPureLogger :: PureLogger (LoggerNameBox m) a
--     } deriving (Functor, Applicative, Monad, MonadState (Seq LogEvent),
--                 MonadThrow, HasLoggerName)

-- instance MonadTrans NamedPureLogger where
--     lift = NamedPureLogger . lift . lift
-- instance Monad m => CanLog (NamedPureLogger m) where
--     dispatchMessage name sev msg =
--         NamedPureLogger $ dispatchMessage name sev msg
-- instance MFunctor NamedPureLogger where
--     hoist f = NamedPureLogger . hoist (hoist f) . runNamedPureLogger

-- runNamedPureLog
--     :: (Monad m, HasLoggerName m)
--     => NamedPureLogger m a -> m (a, [LogEvent])
-- runNamedPureLog (NamedPureLogger action) =
--     askLoggerName >>= (`usingLoggerName` runPureLog action)

dispatchEvents :: CanLog m => [LogEvent] -> m ()
dispatchEvents = mapM_ dispatchLogEvent
  where
    dispatchLogEvent (LogEvent name sev t) = dispatchMessage name sev t

---

{-# NOINLINE loggingHandler #-}
loggingHandler :: MVar Log.LoggingHandler
loggingHandler = unsafePerformIO $ do newEmptyMVar

-- | setup logging according to configuration @LoggerConfig@
--   the backends (scribes) will be registered with katip
setupLogging :: MonadIO m => LoggerConfig -> m ()
setupLogging lc = do
    lh <- liftIO $ Internal.newConfig lc
    scribes <- liftIO $ meta lh lc
    liftIO $ Internal.registerBackends lh scribes
    putMVar loggingHandler lh --Replace with tryPutMVar?
      where
        -- returns a list of: (name, Scribe, finalizer)
        meta :: Log.LoggingHandler -> LoggerConfig -> IO [(Text, K.Scribe)]
        meta _lh _lc = do
            -- setup scribes according to configuration
            let lhs = _lc ^. lcLoggerTree ^. ltHandlers ^.. each
                basepath = _lc ^. lcBasePath
                -- default rotation parameters: max. 24 hours, max. 10 files kept, max. size 5 MB
                rotation = fromMaybe (RotationParameters {_rpMaxAgeHours=24,_rpKeepFilesNum=10,_rpLogLimitBytes=5*1000*1000})
                                     (_lc ^. lcRotation)
            forM lhs (\lh -> case (lh ^. lhBackend) of
                    FileJsonBE -> do
                        let bp = fromMaybe "." basepath
                            fp = fromMaybe "node.json" $ lh ^. lhFpath
                            fdesc = Internal.mkFileDescription bp fp
                            nm = lh ^. lhName
                        scribe <- mkJsonFileScribe
                                      rotation
                                      fdesc
                                      (Internal.sev2klog $ fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (nm, scribe)
                    FileTextBE -> do
                        let bp = fromMaybe "." basepath
                            fp = (fromMaybe "node.log" $ lh ^. lhFpath)
                            fdesc = Internal.mkFileDescription bp fp
                            nm = lh ^. lhName
                        scribe <- mkTextFileScribe
                                      rotation
                                      fdesc
                                      True
                                      (Internal.sev2klog $ fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (nm, scribe)
                    StdoutBE -> do
                        scribe <- mkStdoutScribe
                                      (Internal.sev2klog $ fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (lh ^. lhName, scribe)
                    StderrBE -> do
                        scribe <- mkStderrScribe
                                      (Internal.sev2klog $ fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (lh ^. lhName, scribe)
                    DevNullBE -> do
                        scribe <- mkDevNullScribe _lh
                                      (Internal.sev2klog $ fromMaybe Debug $ lh ^. lhMinSeverity)
                                      K.V0
                        return (lh ^. lhName, scribe)
                 )

instance CanLog IO where
    dispatchMessage name severity msg = do
        lh <- readMVar loggingHandler
        mayEnv <- Internal.getLogEnv lh
        case mayEnv of
            Nothing -> error "logging not yet initialized. Abort."
            Just env -> Log.logItem' ()
                                     (K.Namespace [name])
                                     env
                                     Nothing
                                     (Internal.sev2klog severity)
                                     (K.logStr msg)

test :: IO ()
test = do
    setupLogging $ defaultInteractiveConfiguration Debug
    dispatchMessage "andreas" Info "All good!"
    dispatchMessage "andreas" Info "All good!!"