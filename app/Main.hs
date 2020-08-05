{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Bot
import           EchoBot
import           Logger
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Data.Text                      ( pack )
import           Options
import           Result
import           Misc
import           System.IO                      ( stdout )
import           System.Exit
import qualified Telegram
import qualified Vk

main :: IO ()
main = do
  options <- getOptions
  loglvl  <- resToIO . maybeToRes "Can't parse logLevel" $ maybe
    (Just Warning)
    readT
    (lookup "logLevel" options)
  logOutput <- newMVar stdout
  let log = newLogger logOutput loglvl
      loggedRes r = case r of
          Ok  a -> return a
          Err e -> log Error (pack e) >> exitFailure
  tgopts    <- loggedRes $ mkBotOptions "tg" options
  vkopts    <- loggedRes $ mkBotOptions "vk" options
  echoopts  <- loggedRes $ mkEchoBotOptions options


  defState' <- loggedRes $ defState options
  let program = echoBot echoopts
  log Debug . pack . show $ options

  let tg = case lookup "tg.token" options of
        Just token -> do
          runBot program
                 defState'
                 logOutput
                 "Telegram: "
                 (Telegram.withHandle (pack token))
                 tgopts
        Nothing -> log Info "Telegram token was not found, skipping"

  let vk = case (lookup "vk.token" options, lookup "vk.groupId" options) of
        (Just token, Just group_id) -> do
          runBot program
                 defState'
                 logOutput
                 "Vk: "
                 (Vk.withHandle (pack token) (pack group_id))
                 vkopts
        (_, _) -> log Info "Vkontakte group id or token was not found, skipping"

  _ <- concurrently vk tg
  return ()
