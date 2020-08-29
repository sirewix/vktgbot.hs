{-# LANGUAGE DeriveGeneric #-}
module Vk.Message
    ( Json(..)
    ) where

import           Data.Aeson                     ( FromJSON
                                                , parseJSON
                                                , genericParseJSON
                                                )
import           Data.Text                      ( Text )
import           Deserialization                ( customParseOptions )
import           GHC.Generics                   ( Generic )

data Json = Json
    { _id          :: Int
    , _date        :: Int  -- время отправки в Unixtime.
    , _peer_id     :: Int  -- идентификатор назначения.
    , _from_id     :: Int  -- идентификатор отправителя.
    , _text        :: Text -- текст сообщения.
    , _random_id   :: Int  -- идентификатор, используемый при отправке сообщения. Возвращается только для исходящих сообщений.
    , _important   :: Bool
    } deriving (Generic, Show)

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions

