{-# LANGUAGE DeriveGeneric #-}
module Telegram.Chat
    ( Json(..)
    , CType
    ) where

import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , genericParseJSON
                                                )
import           Deserialization                ( enumParseOptions
                                                , customParseOptions
                                                )
import           GHC.Generics                   ( Generic )

data CType = Private | Group | Supergroup | Channel
    deriving (Generic, Show)

instance FromJSON CType where
    parseJSON = genericParseJSON enumParseOptions

data Json = Json
    { _id         :: Int -- Unique identifier for this chat. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
    , _type       :: CType -- Type of chat, can be either "private", "group", "supergroup" or "channel"
    , _title      :: Maybe String -- Optional. Title, for supergroups, channels and group chats
    , _username   :: Maybe String -- Optional. Username, for private chats, supergroups and channels if available
    , _first_name :: Maybe String -- Optional. First name of the other party in a private chat
    , _last_name  :: Maybe String -- Optional. Last name of the other party in a private chat
    } deriving (Generic, Show)

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions
