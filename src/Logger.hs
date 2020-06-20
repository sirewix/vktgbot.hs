module Logger
  ( Handle(..)
  , Priority(..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  , sublog
  ) where

import Data.Text

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show, Read)

newtype Handle = Handle
    { getLog :: Priority -> Text -> IO () }

logDebug, logInfo, logWarning, logError :: Handle -> Text -> IO ()
logDebug   = (`getLog` Debug)
logInfo    = (`getLog` Info)
logWarning = (`getLog` Warning)
logError   = (`getLog` Error)

sublog :: Text -> Handle -> Handle
sublog prefix logger = Handle
    { getLog = \prio msg ->
        getLog logger prio (prefix <> msg)
    }

