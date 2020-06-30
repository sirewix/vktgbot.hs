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
import Data.List
import Data.Maybe
import Data.Text(Text,pack,unpack,null)
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
import qualified Network.URI.Encode      as U
import qualified Vk.Message              as Message

getLongPollServer log vkpre group_id = do
    longpoll <- vkpre "groups.getLongPollServer" [("group_id" :: String) =: group_id]
    resToIO . eitherToRes . (`A.parseEither` longpoll) . A.withObject "" $ \obj -> do
        key    <- obj .: "key"    :: A.Parser String -- (string) — ключ;
        server <- obj .: "server" :: A.Parser String -- (string) — url сервера;
        ts     <- obj .: "ts"     :: A.Parser String -- (string) — timestamp.
        return (key, server, ts)

--withHandle :: Logger -> BotOptions -> Manager -> (Bot.Bot (Int, Int) -> IO ()) -> IO ()
withHandle log options mgr f = do
    stuff@(key, server, ts) <- getServer
    stuff <- newMVar stuff
    log Info $ "recieved a server and a key | " <> pack server
    let vkpoll serv = vkreq serv parseUpdate ""
    {- apiSendMessage :: !(sesid -> Message -> Maybe [Button] -> IO ())
    ,  apiGetMessages :: !(IO [(sesid, Message)]) -}
    f $ Bot.Bot
        { Bot.apiSendMessage = sendMessage vkpre log group_id
        , Bot.apiGetMessages = getMessages vkpoll log stuff getServer
        }
  where --vkpre :: (A.FromJSON c) => Text -> hz -> IO c
        vkreq = runVk log mgr token proxy
        vkpre = vkreq "https://api.vk.com/method/" parseResult
        proxy = botProxy options
        token = maybe (error "No vkToken found") id (vkToken options)
        group_id = maybe (error "No vkGroupId found") show (vkGroupId options)
        --getServer :: IO (String, String, Int)
        getServer = do
            (key, server, tss) <- getLongPollServer log vkpre group_id
            let (Just ts) = readT tss :: Maybe Int
            {-
            let new_ts = maybe
                        (error "ts string does not contain a valid number")
                        id
                        (readT ts :: Maybe Int)
            -}
            return (key, server, ts)

query :: [(String, String)] -> String
query pairs = "?" <> intercalate "&" (map f pairs)
    where f (k, v) = k <> "=" <> U.encode v

(=:) :: a -> b -> (a, b)
a =: b = (a, b)

runVk log mgr token proxy server parse method params = do
    let params' =
            [ "access_token"  =: token
            , ("v" :: String) =: "5.110" ]
    request <- parseRequest $ server <> unpack method
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

-- parseResult :: (A.FromJSON a) => A.Value -> Result a
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
                , "ts"  =: show ts
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
    let query =
          [ ("group_id" :: String) =: group_id
          , "peer_id" =: (show peer_id)
          , "message" =: unpack text
          , "random_id" =: (show r)
          ]
    vkpre "messages.send" query
    return ()
