{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module EchoBot
  ( echoBot
  , defState
  , EchoBotState
  ) where

import Bot
import Control.Applicative
import Data.Char
import Data.Text(pack,unpack)
import Options
import Misc

newtype EchoBotState = EchoBotState
    { nrepeat :: Int }

defState :: BotOptions -> EchoBotState
defState options = EchoBotState
    { nrepeat = repeatTimes options }

echoBot :: BotOptions -> BotIO EchoBotState ()
echoBot opts = do
    input <- readMessage
    case matchCmd (unpack input) of
      Just (cmd, args) ->
          case command opts cmd of
            Just cmd -> cmd args
            Nothing -> sendMessage ("Unrecognized command \"/" <> pack cmd <> "\", try /help")
      Nothing -> do
          state <- readState
          sequence $ map (const $ sendMessage input) [1..(nrepeat state)]
          return ()

command :: BotOptions -> String -> Maybe (String -> BotIO EchoBotState ())
command opts cmd = pure opts <**>
    case cmd of
      "start"  -> Just help
      "help"   -> Just help
      "repeat" -> Just repeatCmd
      unknown  -> Nothing

help :: BotOptions -> String -> BotIO s ()
help opts _ = sendMessage (helpText opts)

repeatCmd :: BotOptions -> String -> BotIO EchoBotState ()
repeatCmd _ _ = do
    -- sendMessage "How many times do you want your message repeated?"
    sendWithKeyboard  "How many times do you want your message repeated?" (map (pack . show) [1..5])
    msg <- readMessage
    case readT (unpack msg) of
      Just n -> do
          modifyState $ \state -> state { nrepeat = n }
          sendMessage ("The number of repetitions of a message has been set to " <> (pack $ show n))
      Nothing -> sendMessage ("\"" <> msg <> "\" is not a number")


matchCmd :: String -> Maybe (String, String)
matchCmd (first:msg)
  | (first == '/') && (not $ null cmd) = Just (cmd, (dropWhile (==' ') rest))
  | otherwise = Nothing
  where (cmd, rest) = break (not . isAlphaNum) msg
matchCmd [] = Nothing

{-
doubleEchoBot :: BotIO EchoBotState ()
doubleEchoBot = do
    msg1 <- readMessage
    msg2 <- readMessage
    sendMessage ("Pair of messages (" <> msg1 <> ", " <> msg2 <> ")")
-}
