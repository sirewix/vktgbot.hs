{-# LANGUAGE
  DeriveGeneric
, OverloadedStrings
#-}
module Vk.Update
    ( Update(..)
    ) where


import Data.Aeson
import Data.Text(Text)
import GHC.Generics
import SerDe
import qualified Vk.Message as Message
import Data.Aeson.Types

    {-
data Json = Json
    { _type     :: -- тип события
    , _object   :: -- объект, инициировавший событие
--    , _group_id :: -- ID сообщества, в котором произошло событие
    }
    -}

data Update =
    NewMessage Message.Json
  | UnsupportedUpdate

instance FromJSON Update where
    parseJSON = withObject "" $ \upd -> do
        utype <- upd .:  "type" :: Parser Text
        let obj = upd .: "object"
        case utype of
          "message_new" -> NewMessage <$> (obj >>= (.: "message"))
          _ -> return UnsupportedUpdate
