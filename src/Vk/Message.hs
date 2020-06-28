{-# LANGUAGE DeriveGeneric #-}
module Vk.Message
    ( Json(..)
    ) where

import Data.Aeson as Aeson
import GHC.Generics
import Data.Text(Text)

import SerDe

data Json = Json
    { _id      :: Int -- integer	идентификатор сообщения (не возвращается для пересланных сообщений).
    , _user_id :: Int -- integer	идентификатор пользователя, в диалоге с которым находится сообщение.
    , _from_id :: Int -- integer	идентификатор автора сообщения.  положительное число
    , _date    :: Int -- integer	дата отправки сообщения в формате Unixtime.
    , _body    :: Maybe Text -- string	текст сообщения.
    } deriving (Generic, Show)

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions

{-
deleted integer, [0,1]	удалено ли сообщение.
random_id integer	идентификатор, используемый при отправке сообщения. Возвращается только для исходящих сообщений.

Дополнительные поля в сообщениях из мультидиалогов

chat_id integer	идентификатор беседы.
-}
