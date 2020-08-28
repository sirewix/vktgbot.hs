{-# LANGUAGE
   OverloadedStrings
 , ExistentialQuantification
 , FlexibleContexts
 , ScopedTypeVariables
 #-}

module Telegram
  ( newHandle
  )
where

import           Control.Arrow                  ( left )
import           Control.Concurrent             ( newMVar
                                                , putMVar
                                                , takeMVar
                                                )
import           Control.Exception              ( IOException
                                                , catch
                                                , try
                                                )
import           Control.Monad                  ( join )
import           Control.Monad.Except           ( ExceptT(..)
                                                , liftEither
                                                , liftIO
                                                )
import           Data.Aeson                     ( (.=)
                                                , (.:)
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Text.Lazy                 ( toStrict )
import           Data.ByteString.Lazy           ( ByteString )
import           Logger                         ( Logger
                                                , Priority(..)
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

newHandle :: Text -> Logger -> HTTP.Manager -> ExceptT Text IO (Bot.BotAPI (Int, Int))
newHandle token log mgr = liftIO $ do
  let file = "/tmp/tgbotupd"
  contents <- readFile file `catch` \(_ :: IOException) -> return ""
  let offset = fromMaybe 0 (readMaybe contents) :: Int
  upd_offset <- newMVar offset
  return $ Bot.BotAPI
    { Bot.apiSendMessage = \(chat_id, _) -> sendMessage tgpre chat_id
    , Bot.apiGetMessages = getMessages tgpre upd_offset file
    }
 where
  tgpre :: (A.FromJSON c) => Text -> A.Value -> ExceptT Text IO c
  tgpre = runTg log mgr token

runTg log mgr token method body = do
  request <-
    HTTP.parseRequest . unpack $ "https://api.telegram.org/bot" <> token <> "/" <> method
  let req = request
        { HTTP.method         = "POST"
        , HTTP.requestBody    = HTTP.RequestBodyLBS $ A.encode body
        , HTTP.requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }
  res <-
    ExceptT
    $   left (pack . show)
    <$> (try (HTTP.httpLbs req mgr) :: IO
            (Either HTTP.HttpException (HTTP.Response ByteString))
        )
  let resbody = HTTP.responseBody res
  _ <- liftIO $ log Debug $ "recieved " <> (toStrict . E.decodeUtf8 $ A.encode body)
  liftEither $ maybe (Left "body decoding error") parseResult (A.decode resbody)

getMessages tgpre upd_offset file = do
  offset <- liftIO $ takeMVar upd_offset
  updates <- tgpre "getUpdates" $ A.object ["offset" .= offset]
  let msgs = mapMaybe uniqueMsg . startFrom offset $ updates
      new_offset =
        if null updates then offset else Update._update_id . last $ updates
  liftIO $ writeFile file (show new_offset)
  liftIO $ putMVar upd_offset new_offset
  return msgs

-- messages to Text, skip if has no text
uniqueMsg upd = do
  msg <- Update._message upd
  txt <- Message._text msg
  return ((Chat._id . Message._chat $ msg, User._id . Message._from $ msg), txt)

startFrom offset upds@(u : rest) | Update._update_id u <= offset = startFrom offset rest
                                 | otherwise                     = upds
startFrom _ [] = []

sendMessage tgpre chatId text btns = do
  _ <- tgpre "sendMessage" body :: ExceptT Text IO Message.Json
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

parseResult :: (A.FromJSON a) => A.Value -> Either Text a
parseResult v = left pack . join $ A.parseEither parser v
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
