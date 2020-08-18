{-# LANGUAGE DeriveGeneric #-}
module Telegram.User
    ( Json(..)
    ) where

import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , genericParseJSON
                                                )
import           Deserialization                ( customParseOptions )
import           GHC.Generics                   ( Generic )

data Json = Json
  { _id                          :: Int -- Unique identifier for this user or bot
  , _is_bot                      :: Bool -- True, if this user is a bot
  , _first_name                  :: String -- User‘s or bot’s first name
  , _last_name                   :: Maybe String -- Optional. User‘s or bot’s last name
  , _username                    :: Maybe String -- Optional. User‘s or bot’s username
  , _language_code               :: Maybe String -- Optional. IETF language tag of the user's language
  , _can_join_groups             :: Maybe Bool -- Optional. True, if the bot can be invited to groups. Returned only in getMe.
  , _can_read_all_group_messages :: Maybe Bool -- Optional. True, if privacy mode is disabled for the bot. Returned only in getMe.
  , _supports_inline_queries     :: Maybe Bool -- Optional. True, if the bot supports inline queries. Returned only in getMe.
  } deriving (Generic, Show)

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions
