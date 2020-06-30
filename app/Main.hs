{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot
import EchoBot
import Logger
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options
import Result
import Storage
import System.IO(stdout)
import qualified Telegram
import qualified Vk

main :: IO ()
main = do
    options <- getOptions
    log <- newLogger stdout (logLevel options)

    let settings = managerSetProxy
            (proxyEnvironment . botProxy $ options)
            tlsManagerSettings
            -- defaultManagerSettings
        defState' = defState options
        program = echoBot options
    manager <- newManager settings

    tgStorage <- Bot.newStorage -- :: Storage (Int, Int) EchoBotState
    runBot Telegram.withHandle (sublog "Telegram: " log) options manager program tgStorage defState'

    vkStorage <- Bot.newStorage -- :: IO (Bot.Storage Int EchoBotState)
    runBot Vk.withHandle       (sublog "Vk: " log)       options manager program vkStorage defState'
