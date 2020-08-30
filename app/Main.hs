{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Bot                            ( runBot )
import           Bot.Options                    ( mkBotOptions )
import           Control.Concurrent.Async       ( concurrently )
import           Control.Concurrent.MVar        ( newMVar )
import           Control.Monad.Except           ( liftIO
                                                , liftEither
                                                )
import           Data.Text                      ( pack )
import           EchoBot                        ( echoBot )
import           EchoBot.Options                ( mkEchoBotOptions )
import           EchoBot.State                  ( defaultState )
import           Logger                         ( Priority(..)
                                                , newLogger
                                                )
import           Misc                           ( loggedExceptT
                                                , readT
                                                )
import           Options                        ( getOptions )
import           System.IO                      ( stdout )
import qualified Telegram
import qualified Vk

main :: IO ()
main = do
  logOutput <- newMVar stdout
  let bootlog = newLogger logOutput Error
  loggedExceptT bootlog $ do
    options <- getOptions
    loglvl  <- liftEither $ maybe (Left "Can't parse logLevel") Right $ maybe
      (Just Warning)
      readT
      (lookup "logLevel" options)
    let log = newLogger logOutput loglvl
    liftIO . loggedExceptT log $ do
      tgopts    <- liftEither $ mkBotOptions "tg" options
      vkopts    <- liftEither $ mkBotOptions "vk" options
      echoopts  <- liftEither $ mkEchoBotOptions options
      defState' <- liftEither $ defaultState options
      let program = echoBot echoopts
      liftIO . log Debug . pack . show $ options

      let tg = case lookup "tg.token" options of
            Just token -> do
              runBot program
                     defState'
                     logOutput
                     "Telegram: "
                     (Telegram.newAPI token)
                     tgopts
            Nothing -> log Warning "Telegram token was not found, skipping"

      let vk = case (lookup "vk.token" options, lookup "vk.groupId" options) of
            (Just token, Just group_id) -> do
              runBot program
                     defState'
                     logOutput
                     "Vk: "
                     (Vk.newAPI token group_id)
                     vkopts
            (_, _) -> log Warning "Vkontakte group id or token was not found, skipping"

      _ <- liftIO $ concurrently vk tg
      return ()
