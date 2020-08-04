{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Telegram.Message
  ( Json(..)
  )
where

import           Data.Aeson                    as Aeson
import           GHC.Generics
import           SerDe
import qualified Data.Text                     as T
import qualified Telegram.Chat                 as Chat
import qualified Telegram.User                 as User

data Json = Json
  { _message_id :: Int -- Unique message identifier inside this chat
  , _from       :: User.Json -- Optional. Sender, empty for messages sent to channels
  , _date       :: Int -- Date the message was sent in Unix time
  , _chat       :: Chat.Json -- Conversation the message belongs to
  , _text       :: Maybe T.Text -- Optional. For text messages, the actual UTF-8 text of the message, 0-4096 characters
  } deriving (Generic, Show)

instance FromJSON Json where
  parseJSON = genericParseJSON customParseOptions
