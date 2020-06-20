{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Telegram.Message
    ( Json(..)
    ) where

import Data.Aeson as Aeson
import Data.Time.Clock(UTCTime)
import GHC.Generics

import SerDe
import qualified Telegram.User as User
import qualified Telegram.Chat as Chat
import qualified Bot as Bot
import qualified Data.Text as T

    {-
instance Bot.Message Json User.Json where
    getText = _text
    getUser = _from
-}

data Json = Json
    { _message_id :: Int -- Unique message identifier inside this chat
    , _from       :: User.Json -- Optional. Sender, empty for messages sent to channels
    , _date       :: Int -- Date the message was sent in Unix time
    , _chat       :: Chat.Json -- Conversation the message belongs to
    , _text       :: Maybe T.Text -- Optional. For text messages, the actual UTF-8 text of the message, 0-4096 characters

{-
forward_from :: TgUser -- Optional. For forwarded messages, sender of the original message
forward_from_chat :: Chat -- Optional. For messages forwarded from channels, information about the original channel
forward_from_message_id :: Integer -- Optional. For messages forwarded from channels, identifier of the original message in the channel
forward_signature :: String -- Optional. For messages forwarded from channels, signature of the post author if present
forward_sender_name :: String -- Optional. Sender's name for messages forwarded from users who disallow adding a link to their account in forwarded messages
forward_date :: Integer -- Optional. For forwarded messages, date the original message was sent in Unix time
reply_to_message :: Message -- Optional. For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
edit_date :: Integer -- Optional. Date the message was last edited in Unix time
media_group_id :: String -- Optional. The unique identifier of a media message group this message belongs to
author_signature :: String -- Optional. Signature of the post author for messages in channels
entities Array of :: MessageEntity -- Optional. For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
caption_entities Array of :: MessageEntity -- Optional. For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption
audio :: Audio -- Optional. Message is an audio file, information about the file
document :: Document -- Optional. Message is a general file, information about the file
animation :: Animation -- Optional. Message is an animation, information about the animation. For backward compatibility, when this field is set, the document field will also be set
game :: Game -- Optional. Message is a game, information about the game. More about games
photo Array of :: PhotoSize -- Optional. Message is a photo, available sizes of the photo
sticker :: Sticker -- Optional. Message is a sticker, information about the sticker
video :: Video -- Optional. Message is a video, information about the video
voice :: Voice -- Optional. Message is a voice message, information about the file
video_note :: VideoNote -- Optional. Message is a video note, information about the video message
caption :: String -- Optional. Caption for the animation, audio, document, photo, video or voice, 0-1024 characters
contact :: Contact -- Optional. Message is a shared contact, information about the contact
location :: Location -- Optional. Message is a shared location, information about the location
venue :: Venue -- Optional. Message is a venue, information about the venue
poll :: Poll -- Optional. Message is a native poll, information about the poll
dice :: Dice -- Optional. Message is a dice with random value from 1 to 6
new_chat_members Array of :: TgUser -- Optional. New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)
left_chat_member :: TgUser -- Optional. A member was removed from the group, information about them (this member may be the bot itself)
new_chat_title :: String -- Optional. A chat title was changed to this value
new_chat_photo Array of :: PhotoSize -- Optional. A chat photo was change to this value
delete_chat_photo :: True -- Optional. Service message: the chat photo was deleted
group_chat_created :: True -- Optional. Service message: the group has been created
supergroup_chat_created :: True -- Optional. Service message: the supergroup has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a supergroup when it is created. It can only be found in reply_to_message if someone replies to a very first message in a directly created supergroup.
channel_chat_created :: True -- Optional. Service message: the channel has been created. This field can‘t be received in a message coming through updates, because bot can’t be a member of a channel when it is created. It can only be found in reply_to_message if someone replies to a very first message in a channel.
migrate_to_chat_id :: Integer -- Optional. The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
migrate_from_chat_id :: Integer -- Optional. The supergroup has been migrated from a group with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
pinned_message :: Message -- Optional. Specified message was pinned. Note that the Message object in this field will not contain further reply_to_message fields even if it is itself a reply.
invoice :: Invoice -- Optional. Message is an invoice for a payment, information about the invoice. More about payments »
successful_payment :: SuccessfulPayment -- Optional. Message is a service message about a successful payment, information about the payment. More about payments »
connected_website :: String -- Optional. The domain name of the website on which the user has logged in. More about Telegram Login »
passport_data :: PassportData -- Optional. Telegram Passport data
reply_markup :: InlineKeyboardMarkup -- Optional. Inline keyboard attached to the message. login_url buttons are represented as ordinary url buttons.
-}
    } deriving (Generic, Show)

instance FromJSON Json where
    parseJSON = genericParseJSON customParseOptions
