
module Main (main) where

import Lib
import Options
import Result

import Logger
import LoggerImpl
import Telegram
import EchoBot

main :: IO ()
--main = runTg
main = do
    options <- getOptions
    entry options
    return ()

