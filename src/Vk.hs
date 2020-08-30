{-# LANGUAGE OverloadedStrings #-}

module Vk
    ( newAPI
    ) where

import           Bot.API                        ( BotAPI(..)
                                                , Button
                                                )
import           Bot.IO                         ( Message )
import           Control.Arrow                  ( left )
import           Control.Concurrent             ( MVar
                                                , newMVar
                                                , takeMVar
                                                , threadDelay
                                                , putMVar
                                                )
import           Control.Exception              ( try )
import           Control.Monad                  ( (>=>)
                                                , join
                                                )
import           Control.Monad.Except           ( ExceptT(..)
                                                , liftIO
                                                , liftEither
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (.=)
                                                , FromJSON
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Functor                   ( (<&>) )
import           Data.Int                       ( Int64 )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , intercalate
                                                )
import           Data.Text.Lazy                 ( toStrict )
import           Logger                         ( Logger
                                                , Priority(..)
                                                )
import           Misc                           ( (=:)
                                                , readT
                                                )
import           System.Random                  ( randomIO )
import           Vk.Update                      ( Update(..) )
import           Vk.Poll                        ( VkPollResult(..)
                                                , parseUpdate
                                                , getLongPollServer
                                                )
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.Encoding       as LE
import qualified Network.HTTP.Client           as HTTP
import qualified Network.URI.Encode            as URI
import qualified Vk.Message                    as Message

newAPI :: Text -> Text -> Logger -> HTTP.Manager -> ExceptT Text IO (BotAPI Int)
newAPI token group_id log mgr = do
  stuff@(_key, server, _ts) <- getServer
  stuff                     <- liftIO $ newMVar stuff
  _                         <- liftIO $ log Info $ "recieved a server and a key | " <> server
  let vkpoll serv = vkreq serv parseUpdate ""
  return $ BotAPI
      { apiSendMessage = sendMessage vkpre log group_id
      , apiGetMessages = getMessages vkpoll log stuff getServer
      }
 where
  vkreq     = runVk log mgr token
  vkpre     = vkreq "https://api.vk.com/method/" parseResult
  getServer = do
    (key, server, tss) <- getLongPollServer log vkpre group_id
    let (Just ts) = readT tss :: Maybe Int
    return (key, server, ts)

query :: [(Text, Text)] -> Text
query pairs = "?" <> intercalate "&" (map f pairs)
  where f (k, v) = k <> "=" <> URI.encodeText v

runVk
  :: Logger
  -> HTTP.Manager
  -> Text
  -> Text
  -> (A.Value -> Either Text a)
  -> Text
  -> [(Text, Text)]
  -> ExceptT Text IO a
runVk log mgr token server parse method params = do
  let params' = ["access_token" =: token, "v" =: "5.110"]
  request <- HTTP.parseRequest . unpack $ server <> method <> query (params' <> params)
  _       <- liftIO $ log Debug $ "sending " <> pack (show params)
  let req = request
  res <-
    ExceptT
    $   left (pack . show)
    <$> (try (HTTP.httpLbs req mgr) :: IO
            (Either HTTP.HttpException (HTTP.Response ByteString))
        )
  let resbody = HTTP.responseBody res
  _ <- liftIO $ log Debug $ "recieved " <> (toStrict . LE.decodeUtf8 $ resbody)
  liftEither $ maybe (Left "body decoding error") parse (A.decode resbody)

parseResult :: FromJSON a => A.Value -> Either Text a
parseResult = left pack . join . A.parseEither parser
 where
  parser = A.withObject "" $ \obj -> do
    res <- obj .:? "response"
    case res of
      Just x  -> return $ Right x
      Nothing -> do
        err  <- obj .: "error"
        code <- err .: "error_code" :: A.Parser Int
        msg  <- err .: "error_msg"
        return . Left $ "request failed with code " <> show code <> ": " <> msg

getMessages
  :: (Text -> [(Text, Text)] -> ExceptT Text IO VkPollResult)
  -> Logger
  -> MVar (Text, Text, Int)
  -> ExceptT Text IO (Text, Text, Int)
  -> ExceptT Text IO [(Int, Message)]
getMessages vkpoll log stuff getServer = do
 stuff' <- liftIO $ takeMVar stuff
 (stuff'', msgs) <- getMessages' stuff'
 liftIO $ putMVar stuff stuff''
 return msgs
 where
  getMessages' (key, server, ts) = do
    updates <- vkpoll
      server
      [ "act" =: "a_check"
      , "key" =: key
      , "ts" =: pack (show ts)
      ]
    case updates of
      VkOk new_ts updates -> do
        _ <- liftIO $ log Debug $ "new ts: " <> pack (show new_ts)
        let msgs = mapMaybe (combMessages >=> toBotMsg) updates
        return ((key, server, new_ts), msgs)
      RenewTs new_ts -> getMessages' (key, server, new_ts)
      PollFail       -> liftIO (threadDelay 30000000) >> getServer >>= getMessages'
  combMessages UnsupportedUpdate = Nothing
  combMessages (NewMessage msg)  = Just msg
  toBotMsg msg = do
    let txt = Message._text msg
    if T.null txt then Nothing else Just (Message._peer_id msg, txt)

sendMessage
  :: FromJSON a
  => (Text -> [(Text, Text)] -> ExceptT Text IO a)
  -> Logger
  -> Text
  -> Int
  -> Text
  -> Maybe [Button]
  -> ExceptT Text IO ()
sendMessage vkpre _log group_id peer_id text btns = do
  r <- liftIO randomIO
  let query = mbkeyboard
        [ "group_id" =: group_id
        , "peer_id" =: pack (show peer_id)
        , "message" =: text
        , "random_id" =: pack (show (r :: Int64))
        ]
  _ <- vkpre "messages.send" query
  return ()
 where
  mbkeyboard = consMay "keyboard" (toStrict . LE.decodeUtf8 . A.encode <$> keyboard)
  consMay attr = maybe id ((:) . (attr =:))
  keyboard = btns <&> \btns ->
    A.object ["one_time" .= True, "inline" .= False, "buttons" .= [map button btns]]

button :: Button -> A.Value
button b = A.object
  [ "action" .= A.object ["type" .= ("text" :: Text), "label" .= b]
  , "color" .= ("secondary" :: Text)
  ]
