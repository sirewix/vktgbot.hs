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
    { _id          :: Int -- integer	идентификатор сообщения.
    , _date        :: Int -- integer	время отправки в Unixtime.
    , _peer_id     :: Int -- integer	идентификатор назначения.
    , _from_id     :: Int -- integer	идентификатор отправителя.
    , _text        :: Text -- string	текст сообщения.
    , _random_id   :: Int  -- integer	идентификатор, используемый при отправке сообщения. Возвращается только для исходящих сообщений.
    , _important   :: Bool -- boolean	true, если сообщение помечено как важное.
    } deriving (Generic, Show)

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions

