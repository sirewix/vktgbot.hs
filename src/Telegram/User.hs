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
  { _id                          :: Int
  , _is_bot                      :: Bool
  , _first_name                  :: String       -- User‘s or bot’s first name
  , _last_name                   :: Maybe String -- User‘s or bot’s last name
  , _username                    :: Maybe String -- User‘s or bot’s username
  , _language_code               :: Maybe String -- IETF language tag of the user's language
  } deriving (Generic, Show)

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions
