module Logging where

import Katip

data LogLevel = Debug | Info | None deriving (Show, Read)

toLogSeverity :: LogLevel -> Severity
toLogSeverity Debug = DebugS
toLogSeverity Info = InfoS
toLogSeverity None = EmergencyS
