{-# LANGUAGE
  DeriveDataTypeable
, OverloadedStrings
  #-}
module Logger
  ( Priority(..)
  , Logger
  , sublog
  , newLogger
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Data                      ( Data )
import           Data.Text                      ( Text, pack)
import           Data.Text.IO                   ( hPutStrLn )
import           Data.Time.Clock
import qualified System.IO

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show, Read, Data)

type Logger = Priority -> Text -> IO ()

sublog :: Text -> Logger -> Logger
sublog prefix logger prio msg = logger prio (prefix <> msg)

newLogger :: MVar System.IO.Handle -> Priority -> Logger
newLogger h logPrio prio msg = when (prio >= logPrio) $ do
  now <- getCurrentTime
  withMVar h $ \h ->
    hPutStrLn h $ (pack . show) now <> " [" <> (pack . show) prio <> "] " <> msg
