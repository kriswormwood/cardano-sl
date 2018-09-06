-- | Server part.

module Pos.Communication.Server
       ( serverLoggerName
       ) where

import           Pos.Util.Wlog (LoggerName)

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
