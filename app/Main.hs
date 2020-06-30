{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot
import EchoBot
import Logger
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Concurrent.Async
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

    let vk = case tgToken options of
              Just token -> do
                tgStorage <- Bot.newStorage
                runBot (Telegram.withHandle token) (sublog "Telegram: " log) manager program tgStorage defState'
              Nothing ->
                log Info "Telegram token was not found, skipping"

    let tg = case (vkToken options, vkGroupId options) of
              (Just token, Just group_id) -> do
                vkStorage <- Bot.newStorage
                runBot (Vk.withHandle token group_id)    (sublog "Vk: " log) manager program vkStorage defState'
              (_, _) ->
                log Info "Vkontakte group id or token was not found, skipping"

    concurrently vk tg
    return ()
