module Logger
  ( Priority(..)
  , Logger
  , sublog
  , newLogger
  ) where

import Control.Concurrent
import Data.Text(Text,pack)
import Data.Text.IO(hPutStrLn)
import qualified System.IO as System.IO

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show, Read)

type Logger = Priority -> Text -> IO ()

sublog :: Text -> Logger -> Logger
sublog prefix logger = \prio msg -> logger prio (prefix <> msg)

newLogger :: MVar System.IO.Handle -> Priority -> Logger
newLogger h logPrio = \prio msg ->
    if prio >= logPrio then
        withMVar h $ \h ->
            hPutStrLn h $ (pack $ '[' : show prio ++ "] ")  <>  msg
    else return ()
