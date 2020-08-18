{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module EchoBot
  ( EchoBotState(..)
  , EchoBotOptions(..)
  , defState
  , echoBot
  , mkEchoBotOptions
  )
where

import           Bot                            ( BotIO
                                                , modifyState
                                                , readMessage
                                                , readState
                                                , sendMessage
                                                , sendWithKeyboard
                                                )
import           Control.Applicative            ( (<**>) )
import           Data.Char                      ( isAlphaNum )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Options                        ( Opt )
import           Result                         ( Result
                                                , maybeToRes
                                                )
import           Text.Read                      ( readMaybe )

newtype EchoBotState = EchoBotState
    { nrepeat :: Int }
    deriving (Eq, Show)

newtype EchoBotOptions = EchoBotOptions
    { helpText :: Text }

mkEchoBotOptions :: [Opt] -> Result EchoBotOptions
mkEchoBotOptions opts = pure
  $ EchoBotOptions { helpText = maybe helptxt pack $ lookup "helpText" opts }
 where
  helptxt =
    "Hi, this is simple echo bot" <> "/help — this message" <> "/repeat — set repeat"

defState :: [Opt] -> Result EchoBotState
defState opts = do
  nrepit <- opt "repeatTimes" 5
  return $ EchoBotState { nrepeat = nrepit }
 where
  opt k def = maybeToRes ("Unexpected " <> k) $ maybe (Just def) readMaybe (lookup k opts)

echoBot :: EchoBotOptions -> BotIO EchoBotState ()
echoBot opts = do
  input <- readMessage
  case matchCmd (unpack input) of
    Just (cmd, args) -> case command opts cmd of
      Just cmd -> cmd args
      Nothing  -> sendMessage ("Unrecognized command \"/" <> pack cmd <> "\", try /help")
    Nothing -> do
      state <- readState
      mapM_ (const $ sendMessage input) [1 .. (nrepeat state)]

command :: EchoBotOptions -> String -> Maybe (String -> BotIO EchoBotState ())
command opts cmd = pure opts <**> case cmd of
  "start"  -> Just help
  "help"   -> Just help
  "repeat" -> Just repeatCmd
  _        -> Nothing

help :: EchoBotOptions -> String -> BotIO s ()
help opts _ = sendMessage (helpText opts)

repeatCmd :: EchoBotOptions -> String -> BotIO EchoBotState ()
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


matchCmd :: String -> Maybe (String, String)
matchCmd (first : msg)
  | (first == '/') && not (null cmd) = Just (cmd, dropWhile (== ' ') rest)
  | otherwise                          = Nothing
  where (cmd, rest) = span isAlphaNum msg
matchCmd [] = Nothing
