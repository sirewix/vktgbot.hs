{-# LANGUAGE DeriveGeneric #-}
module Telegram.Chat
    ( Json(..)
    , CType
    ) where

import Data.Aeson
import GHC.Generics

import SerDe

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
{-
photo :: ChatPhoto -- Optional. Chat photo. Returned only in getChat.
description :: String -- Optional. Description, for groups, supergroups and channel chats. Returned only in getChat.
invite_link :: String -- Optional. Chat invite link, for groups, supergroups and channel chats. Each administrator in a chat generates their own invite links, so the bot must first generate the link using exportChatInviteLink. Returned only in getChat.
pinned_message :: Message -- Optional. Pinned message, for groups, supergroups and channels. Returned only in getChat.
permissions :: ChatPermissions -- Optional. Default chat member permissions, for groups and supergroups. Returned only in getChat.
slow_mode_delay :: Integer -- Optional. For supergroups, the minimum allowed delay between consecutive messages sent by each unpriviledged user. Returned only in getChat.
sticker_set_name :: String -- Optional. For supergroups, name of group sticker set. Returned only in getChat.
can_set_sticker_set :: Boolean -- Optional. True, if the bot can change the group sticker set. Returned only in getChat.
-}
    } deriving (Generic, Show)

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions
