{-# LANGUAGE
  OverloadedStrings
, FlexibleContexts
#-}

module Vk
    ( withHandle
    ) where

--import qualified Data.HashMap.Lazy       as Map
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson((.:),(.:?),(.=))
import Data.Int
import Data.Functor
-- import Data.List
import Data.Maybe
import Data.Text(Text,pack,unpack,null,intercalate)
import Data.Text.Lazy(toStrict)
import GHC.Generics
import Logger
import Misc
import Network.HTTP.Client
import Options
import Result
import SerDe
import System.Random
import Vk.Update
import qualified Bot                     as Bot
import qualified Data.Aeson              as A
import qualified Data.Aeson.Types        as A
import qualified Data.Text               as T
import qualified Data.Text.Lazy.Encoding as LE
import qualified Network.URI.Encode      as URI
import qualified Vk.Message              as Message

getLongPollServer log vkpre group_id = do
    longpoll <- vkpre "groups.getLongPollServer" ["group_id" =: group_id]
    resToIO . eitherToRes . (`A.parseEither` longpoll) . A.withObject "" $ \obj -> do
        key    <- obj .: "key"    :: A.Parser Text -- (string) — ключ;
        server <- obj .: "server" :: A.Parser Text -- (string) — url сервера;
        ts     <- obj .: "ts"     :: A.Parser String -- (string) — timestamp.
        return (key, server, ts)

withHandle token group_id log mgr f = do
    stuff@(key, server, ts) <- getServer
    stuff <- newMVar stuff
    log Info $ "recieved a server and a key | " <> server
    let vkpoll serv = vkreq serv parseUpdate ""
    f $ Bot.Bot
        { Bot.apiSendMessage = sendMessage vkpre log group_id
        , Bot.apiGetMessages = getMessages vkpoll log stuff getServer
        }
  where vkreq = runVk log mgr token
        vkpre = vkreq "https://api.vk.com/method/" parseResult
        getServer = do
            (key, server, tss) <- getLongPollServer log vkpre group_id
            let (Just ts) = readT tss :: Maybe Int
            return (key, server, ts)

query :: [(Text, Text)] -> Text
query pairs = "?" <> intercalate "&" (map f pairs)
    where f (k, v) = k <> "=" <> URI.encodeText v

runVk log mgr token server parse method params = do
    let params' =
            [ "access_token"  =: token
            , "v" =: "5.110" ]
    request <- parseRequest . unpack $ server <> method
                                              <> query (params' <> params)
    log Debug $ "sending " <> (pack $ show params)
    let req = request
    res <- httpLbs req mgr
    let resbody = responseBody res

    log Debug $ "recieved " <> (toStrict . LE.decodeUtf8  $ resbody)
    resToM $ maybe
        (error "Decoding error")
        parse
        (A.decode resbody)

parseResult v = eitherToRes =<< (eitherToRes . A.parseEither parser $ v)
    where parser = A.withObject "" $ \obj -> do
             res <- obj .:?  "response"
             case res of
               Just x -> return $ Right x
               Nothing -> do
                   err  <- obj .: "error"
                   code <- err .: "error_code" :: A.Parser Int
                   msg  <- err .: "error_msg"
                   return . Left $ "request failed with code " <> show code <> ": " <> msg

data VkPollResult =
    VkOk Int [Update]
  | RenewTs Int
  | PollFail

parseUpdate :: A.Value -> Result VkPollResult
parseUpdate = eitherToRes . A.parseEither parser
    where parser = A.withObject "" $ \obj -> do
             failed <- obj .:? "failed" :: A.Parser (Maybe Int)
             case failed of
               Just 1 -> do
                   tss <- obj .: "ts"
                   let (Just ts) = readT tss :: Maybe Int
                   return (RenewTs ts)
               Just _ -> do
                   return PollFail
               Nothing -> do
                   tss <- obj .: "ts"
                   let (Just ts) = readT tss :: Maybe Int
                   updates <- obj .: "updates"
                   return $ VkOk ts updates

getMessages vkpoll log stuff getServer = modifyMVar stuff getMessages'
    where getMessages' (key, server, ts) = do
            updates <- vkpoll server
                [ "act" =: "a_check"
                , "key" =: key
                , "ts"  =: (pack $ show ts)
             -- , "wait" =: "25"
                ]
            case updates of
              VkOk new_ts updates -> do
                  log Debug $ "new ts: " <> (pack $ show new_ts)
                  let msgs = mapMaybe (\u -> combMessages u >>= toBotMsg) updates
                  return ((key, server, new_ts), msgs)
              RenewTs new_ts -> getMessages' (key, server, new_ts)
              PollFail -> getServer >>= getMessages'
          combMessages UnsupportedUpdate = Nothing
          combMessages (NewMessage msg) = Just msg
          toBotMsg msg = do
              let txt = Message._text msg
              if T.null txt then Nothing
                          else Just (Message._peer_id msg, txt)

sendMessage vkpre log group_id peer_id text btns = do
    r <- randomIO :: IO Int64
    let query = mbkeyboard
          [ "group_id" =: group_id
          , "peer_id" =: (pack $ show peer_id)
          , "message" =: text
          , "random_id" =: (pack $ show r) ]
    vkpre "messages.send" query
    return ()
  where
      mbkeyboard = consMay "keyboard" (toStrict . LE.decodeUtf8 <$> A.encode <$> keyboard)
      consMay attr = maybe id ((:) . (attr =:))
      keyboard = btns <&> \btns -> A.object
          [ "one_time" .= True
          , "inline" .= False
          , "buttons" .= [map button btns]
          ]

button :: Bot.Button -> A.Value
button b = A.object
    [ "action" .= A.object
        [ "type" .= ("text" :: Text)
        , "label" .= b ]
    , "color" .= ("secondary" :: Text) ]
