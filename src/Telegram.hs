{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram
    ( withHandle
    ) where

-- import Network.HTTP.Req
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson((.=))
import Data.Functor
import Data.Maybe
import Data.Text(Text,pack,unpack)
import Data.Text.Internal.Builder
import Data.Text.Lazy(toStrict)
import Data.Typeable
import Logger
import Misc
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Options
import Result
import System.Environment
import System.Exit
import System.IO
import Telegram.Result
import qualified Bot
import qualified Data.Aeson              as A
import qualified Data.Text.Encoding      as E
import qualified Data.Text.IO            as I
import qualified Data.Text.Lazy.Encoding as LE
import qualified Telegram.Chat           as Chat
import qualified Telegram.Message        as Message
import qualified Telegram.Result         as Result
import qualified Telegram.Update         as Update
import qualified Telegram.User           as User

withHandle :: Logger -> BotOptions -> Manager -> (Bot.Bot (Int, Int) -> IO ()) -> IO ()
withHandle log options mgr f = do
    let file = "/tmp/tgbotupd"
    contents <- readFile file `catch`
        \e -> const (return "") (e :: IOException)
    let offset = maybe 0 id (readT contents) :: Int
    upd_offset <- newMVar offset
    -- print ("read offset from file = " ++ show offset)
    seq (length contents) $ f $ Bot.Bot
        { Bot.apiSendMessage = \(chat_id, _) -> sendMessage tgpre chat_id
        , Bot.apiGetMessages = getMessages tgpre upd_offset file
        }
  where tgpre :: (A.FromJSON c) => Text -> A.Value -> IO c
        tgpre = runTg log mgr token (botProxy options)
        token = maybe (error "No tgToken found") id (tgToken options)

-- runTg :: (A.FromJSON a) => Logger -> Manager -> String -> p -> Text -> A.Value -> IO a
runTg log mgr token proxy method body = do
    request <- parseRequest $ "https://api.telegram.org/bot" <> token <> "/" <> unpack method
    let req = request
            { method = "POST"
            , requestBody = RequestBodyLBS $ A.encode body
            , requestHeaders =
                [ ("Content-Type", "application/json; charset=utf-8") ]
            }
    res <- httpLbs req mgr
    {-let status  = responseStatus res
        code    = statusCode status
        statmsg = statusMessage status -}
    let resbody = responseBody res

    log Debug $ "recieved " <> (toStrict . LE.decodeUtf8 $ A.encode body)
    resToM $ maybe
        (Err "Decoding error")
        parseTgResult
        (A.decode resbody)

getMessages tgpre upd_offset file = do
    modifyMVar upd_offset $ \upd_offset -> do
        updates <- tgpre "getUpdates" $ A.object [ "offset" .= upd_offset]
        let msgs = mapMaybe uniqueMsg . startFrom upd_offset $ updates
            new_offset = if null updates then upd_offset
                         else (Update._update_id . last $ updates)
        writeFile file (show new_offset)
        return (new_offset, msgs)

-- messages to Text, skip if has no text
uniqueMsg upd = do
    msg <- Update._message upd
    txt <- Message._text msg
    return (( Chat._id . Message._chat $ msg
            , User._id . Message._from $ msg
            ) , txt)

startFrom offset upds@(u:rest)
    | Update._update_id u <= offset = startFrom offset rest
    | otherwise = upds
startFrom _ [] = []

sendMessage tgpre chatId text btns = do
    tgpre "sendMessage" body :: IO Message.Json
    return ()
  where body = A.object $ conss
            [ "chat_id" .= (chatId :: Int) -- Integer or String -- Unique identifier for the target chat
                                           -- or username of the target channel (in the format @channelusername)
            , "text" .= (text :: Text) ] -- String -- Text of the message to be sent, 1-4096 characters after entities parsing
        conss = consMay "reply_markup" keyboard
        keyboard = btns <&> \btns -> A.object
            [ "keyboard" .= [map button btns]
            , "resize_keyboard" .= True ]
        consMay attr = maybe id ((:) . (attr .=))

button :: Bot.Button -> A.Value
button b = A.object [ "text" .= b ]
