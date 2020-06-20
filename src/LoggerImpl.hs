-- | A logger implementation that logs all messages to a 'System.IO.Handle'.
{-# LANGUAGE OverloadedStrings #-}
module LoggerImpl
  ( newHandle
  ) where

import Control.Concurrent.MVar(newMVar,withMVar)
import Data.Text(Text,pack)
import Data.Text.IO(hPutStrLn)
import qualified System.IO as System.IO
import Logger

newHandle :: System.IO.Handle -> Priority -> IO Handle
newHandle fileH logPrio = do
    h <- newMVar fileH

    return $ Handle
      { getLog = \prio msg ->
            if prio >= logPrio then
                withMVar h $ \h ->
                    hPutStrLn h $ (pack $ '[' : show prio ++ "] ")  <>  msg
            else return ()
      }
