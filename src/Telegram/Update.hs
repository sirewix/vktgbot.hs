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
    { _update_id           :: Int -- Update identifiers start from a certain positive number and increase sequentially.
    , _message             :: Maybe Message.Json -- New incoming message of any kind — text, photo, sticker, etc.
    , _edited_message      :: Maybe Message.Json -- New version of a message that is known to the bot and was edited
    , _channel_post        :: Maybe Message.Json -- New incoming channel post of any kind — text, photo, sticker, etc.
    , _edited_channel_post :: Maybe Message.Json -- New version of a channel post that is known to the bot and was edited
    } deriving (Generic, Show)

instance FromJSON Json where
  parseJSON = genericParseJSON customParseOptions
