--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module EchoBot
  ( EchoBotState(..)
  , EchoBotOptions(..)
  , echoBot
  )
where

import           Bot.IO                         ( BotIO
                                                , modifyState
                                                , readMessage
                                                , readState
                                                , sendMessage
                                                , sendWithKeyboard
                                                )
import           Control.Applicative            ( (<**>) )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           EchoBot.Options                ( EchoBotOptions(..) )
import           EchoBot.State                  ( EchoBotState(..) )
import           Text.Read                      ( readMaybe )

import           Text.Parsec                    ( char
                                                , alphaNum
                                                , eof
                                                , many
                                                , many1
                                                , parse
                                                , skipMany
                                                )
import           Text.Parsec.Char               ( anyChar )

echoBot :: EchoBotOptions -> BotIO EchoBotState ()
echoBot opts = do
  input <- readMessage
  case matchCmd input of
    Just (cmd, args) -> case command opts cmd of
      Just cmd -> cmd args
      Nothing  -> sendMessage ("Unrecognized command \"/" <> cmd <> "\", try /help")
    Nothing -> do
      state <- readState
      mapM_ (const $ sendMessage input) [1 .. (nrepeat state)]

command :: EchoBotOptions -> Text -> Maybe (Text -> BotIO EchoBotState ())
command opts cmd = pure opts <**> case cmd of
  "start"  -> Just help
  "help"   -> Just help
  "repeat" -> Just repeatCmd
  _        -> Nothing

help :: EchoBotOptions -> Text -> BotIO s ()
help opts _ = sendMessage (helpText opts)

repeatCmd :: EchoBotOptions -> Text -> BotIO EchoBotState ()
repeatCmd _ _ = do
  state <- readState
  sendWithKeyboard
    (  "How many times do you want your message repeated? Now it is "
    <> (pack . show $ nrepeat state)
    )
    (map (pack . show) ([1 .. 5] :: [Int]))
  msg <- readMessage
  case readMaybe (unpack msg) of
    Just n -> do
      modifyState $ \state -> state { nrepeat = n }
      sendMessage
        ("The number of repetitions of a message has been set to " <> pack (show n))
    Nothing -> sendMessage ("\"" <> msg <> "\" is not a number")

matchCmd :: Text -> Maybe (Text, Text)
matchCmd = either (const Nothing) Just . parse parser ""
 where
  parser = do
    _ <- char '/'
    cmd <- many1 alphaNum
    skipMany (char ' ')
    arg <- many anyChar
    eof
    return (pack cmd, pack arg)
