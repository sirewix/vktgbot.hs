{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module EchoBot
  ( echoBot
  , defState
  , EchoBotState
  , mkEchoBotOptions
  ) where

import Bot
import Control.Applicative
import Data.Char
import Data.Text(Text,pack,unpack)
import Options
import Result
import Misc

newtype EchoBotState = EchoBotState
    { nrepeat :: Int }

newtype EchoBotOptions = EchoBotOptions
    { helpText :: Text }

mkEchoBotOptions :: [Opt] -> Result EchoBotOptions
mkEchoBotOptions opts = pure $ EchoBotOptions
    { helpText = maybe "I don't know who am I" pack $ lookup "helpText" opts }

defState :: [Opt]  -> Result EchoBotState
defState opts = do
    nrepit <- opt "repeatTimes" 5
    return $ EchoBotState
        { nrepeat = nrepit }
    where opt k def = maybeToRes ("Unexpected " <> k) $ maybe (Just def) readT (lookup k opts)

echoBot :: EchoBotOptions -> BotIO EchoBotState ()
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

command :: EchoBotOptions -> String -> Maybe (String -> BotIO EchoBotState ())
command opts cmd = pure opts <**>
    case cmd of
      "start"  -> Just help
      "help"   -> Just help
      "repeat" -> Just repeatCmd
      unknown  -> Nothing

help :: EchoBotOptions -> String -> BotIO s ()
help opts _ = sendMessage (helpText opts)

repeatCmd :: EchoBotOptions -> String -> BotIO EchoBotState ()
repeatCmd _ _ = do
    -- sendMessage "How many times do you want your message repeated?"
    state <- readState
    sendWithKeyboard ("How many times do you want your message repeated? Now it is "
                    <> (pack . show $ nrepeat state)) (map (pack . show) [1..5])
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
