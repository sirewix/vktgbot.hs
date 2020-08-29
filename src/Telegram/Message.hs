{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Telegram.Message
  ( Json(..)
  )
where

import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , genericParseJSON
                                                )
import           Deserialization                ( customParseOptions )
import           GHC.Generics                   ( Generic )
import qualified Data.Text                     as T
import qualified Telegram.Chat                 as Chat
import qualified Telegram.User                 as User

data Json = Json
  { _message_id :: Int
  , _from       :: Maybe User.Json
  , _date       :: Int          -- Date the message was sent in Unix time
  , _chat       :: Chat.Json
  , _text       :: Maybe T.Text -- For text messages, the actual UTF-8 text of the message, 0-4096 characters
  } deriving (Generic, Show)

instance FromJSON Json where
  parseJSON = genericParseJSON customParseOptions
