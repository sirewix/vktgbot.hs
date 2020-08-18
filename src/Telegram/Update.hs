{-# LANGUAGE DeriveGeneric #-}
module Telegram.Update
  ( Json(..)
  )
where

import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , genericParseJSON
                                                )
import           Deserialization                ( customParseOptions )
import           GHC.Generics                   ( Generic )
import qualified Telegram.Message              as Message

data Json = Json
    { _update_id           :: Int -- Integer The update‘s unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using Webhooks, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. If there are no new updates for at least a week, then identifier of the next update will be chosen randomly instead of sequentially.
    , _message             :: Maybe Message.Json -- Optional. New incoming message of any kind — text, photo, sticker, etc.
    , _edited_message      :: Maybe Message.Json -- Optional. New version of a message that is known to the bot and was edited
    , _channel_post        :: Maybe Message.Json -- Optional. New incoming channel post of any kind — text, photo, sticker, etc.
    , _edited_channel_post :: Maybe Message.Json -- Optional. New version of a channel post that is known to the bot and was edited
    } deriving (Generic, Show)

instance FromJSON Json where
  parseJSON = genericParseJSON customParseOptions
