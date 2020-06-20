{-# LANGUAGE DeriveGeneric #-}
module Telegram.Update
    ( Json(..)
    ) where

import Data.Aeson as Aeson
import GHC.Generics

import SerDe
import qualified Telegram.Message as Message

data Json = Json
    { _update_id           :: Int -- Integer The update‘s unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using Webhooks, since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order. If there are no new updates for at least a week, then identifier of the next update will be chosen randomly instead of sequentially.
    , _message             :: Maybe Message.Json -- Optional. New incoming message of any kind — text, photo, sticker, etc.
    , _edited_message      :: Maybe Message.Json -- Optional. New version of a message that is known to the bot and was edited
    , _channel_post        :: Maybe Message.Json -- Optional. New incoming channel post of any kind — text, photo, sticker, etc.
    , _edited_channel_post :: Maybe Message.Json -- Optional. New version of a channel post that is known to the bot and was edited
{-
inline_query InlineQuery -- Optional. New incoming inline query
chosen_inline_result ChosenInlineResult -- Optional. The result of an inline query that was chosen by a user and sent to their chat partner. Please see our documentation on the feedback collecting for details on how to enable these updates for your bot.
callback_query CallbackQuery -- Optional. New incoming callback query
shipping_query ShippingQuery -- Optional. New incoming shipping query. Only for invoices with flexible price
pre_checkout_query PreCheckoutQuery -- Optional. New incoming pre-checkout query. Contains full information about checkout
poll Poll -- Optional. New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot
poll_answer PollAnswer -- Optional. A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself.
-}
    } deriving (Generic, Show)

-- getTgUser _id user

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions
