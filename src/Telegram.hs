{-# LANGUAGE
   OverloadedStrings
 , ExistentialQuantification
 , FlexibleContexts
 , ScopedTypeVariables
 #-}

module Telegram
  ( withHandle
  )
where

import           Control.Concurrent             ( newMVar
                                                , modifyMVar
                                                )
import           Control.Exception              ( IOException
                                                , catch
                                                )
import           Data.Aeson                     ( (.=)
                                                , (.:)
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Text.Lazy                 ( toStrict )
import           Logger                         ( Logger
                                                , Priority(..)
                                                )
import           Result                         ( Result(..)
                                                , resToM
                                                , eitherToRes
                                                )
import           Text.Read                      ( readMaybe )
import qualified Bot
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import qualified Data.Text.Lazy.Encoding       as E
import qualified Network.HTTP.Client           as HTTP
import qualified Telegram.Chat                 as Chat
import qualified Telegram.Message              as Message
import qualified Telegram.Update               as Update
import qualified Telegram.User                 as User

withHandle :: Text -> Logger -> HTTP.Manager -> (Bot.Bot (Int, Int) -> IO ()) -> IO ()
withHandle token log mgr f = do
  let file = "/tmp/tgbotupd"
  contents <- readFile file `catch` \(_ :: IOException) -> return ""
  let offset = fromMaybe 0 (readMaybe contents) :: Int
  upd_offset <- newMVar offset
  seq (length contents) $ f $ Bot.Bot
    { Bot.apiSendMessage = \(chat_id, _) -> sendMessage tgpre chat_id
    , Bot.apiGetMessages = getMessages tgpre upd_offset file
    }
 where
  tgpre :: (A.FromJSON c) => Text -> A.Value -> IO c
  tgpre = runTg log mgr token

runTg log mgr token method body = do
  request <-
    HTTP.parseRequest . unpack $ "https://api.telegram.org/bot" <> token <> "/" <> method
  let req = request
        { HTTP.method         = "POST"
        , HTTP.requestBody    = HTTP.RequestBodyLBS $ A.encode body
        , HTTP.requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }
  res <- HTTP.httpLbs req mgr
  let resbody = HTTP.responseBody res
  _ <- log Debug $ "recieved " <> (toStrict . E.decodeUtf8 $ A.encode body)
  resToM $ maybe (Err "Decoding error") parseResult (A.decode resbody)

getMessages tgpre upd_offset file = do
  modifyMVar upd_offset $ \upd_offset -> do
    updates <- tgpre "getUpdates" $ A.object ["offset" .= upd_offset]
    let msgs = mapMaybe uniqueMsg . startFrom upd_offset $ updates
        new_offset =
          if null updates then upd_offset else Update._update_id . last $ updates
    writeFile file (show new_offset)
    return (new_offset, msgs)

-- messages to Text, skip if has no text
uniqueMsg upd = do
  msg <- Update._message upd
  txt <- Message._text msg
  return ((Chat._id . Message._chat $ msg, User._id . Message._from $ msg), txt)

startFrom offset upds@(u : rest) | Update._update_id u <= offset = startFrom offset rest
                                 | otherwise                     = upds
startFrom _ [] = []

sendMessage tgpre chatId text btns = do
  _ <- tgpre "sendMessage" body :: IO Message.Json
  return ()
 where
  body = A.object $ conss
    [ "chat_id" .= (chatId :: Int) -- Integer or String -- Unique identifier for the target chat
                                     -- or username of the target channel (in the format @channelusername)
    , "text" .= (text :: Text)
    ] -- String -- Text of the message to be sent, 1-4096 characters after entities parsing
  conss    = consMay "reply_markup" keyboard
  keyboard = btns
    <&> \btns -> A.object ["keyboard" .= [map button btns], "resize_keyboard" .= True]
  consMay attr = maybe id ((:) . (attr .=))

button :: Bot.Button -> A.Value
button b = A.object ["text" .= b]

parseResult :: (A.FromJSON a) => A.Value -> Result.Result a
parseResult v = eitherToRes =<< (eitherToRes . A.parseEither parser $ v)
 where
  parser = A.withObject "" $ \obj -> do
    ok <- obj .: "ok"
    if ok
      then do
        res <- obj .: "result"
        return . Right $ res
      else do
        err <- obj .: "description"
        return . Left $ "request failed with " <> err
