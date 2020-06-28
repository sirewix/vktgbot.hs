module Logger
  ( Priority(..)
  , Logger
  , sublog
  , newLogger
  ) where

import Control.Concurrent.MVar(newMVar,withMVar)
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

newLogger :: System.IO.Handle -> Priority -> IO Logger
newLogger fileH logPrio = do
    h <- newMVar fileH
    return $ \prio msg ->
        if prio >= logPrio then
            withMVar h $ \h ->
                hPutStrLn h $ (pack $ '[' : show prio ++ "] ")  <>  msg
        else return ()
