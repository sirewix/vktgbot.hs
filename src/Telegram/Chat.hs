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
    { _id         :: Int -- Unique identifier for this chat
    , _type       :: CType
    , _title      :: Maybe String -- Title, for supergroups, channels and group chats
    , _username   :: Maybe String -- Username, for private chats, supergroups and channels if available
    , _first_name :: Maybe String -- First name of the other party in a private chat
    , _last_name  :: Maybe String -- Last name of the other party in a private chat
    } deriving (Generic, Show)

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions
