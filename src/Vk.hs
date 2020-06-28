{-# LANGUAGE
  OverloadedStrings
#-}

module Vk
    ( withHandle
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson((.:),(.:?),(.=))
import Data.Text(Text,pack,unpack)
import Data.List
import GHC.Generics
import Logger
import Data.Maybe
import Misc
import Network.HTTP.Client
import Options
import Result
import SerDe
import Vk.Update
import qualified Bot                     as Bot
import qualified Vk.Message              as Message
import qualified Data.Aeson              as A
import qualified Data.Aeson.Types        as A
--import qualified Data.HashMap.Lazy       as Map
import qualified Data.Text.Lazy.Encoding as LE
import qualified Network.URI.Encode      as U

getLongPollServer log vkpre group_id = do
    longpoll <- vkpre "groups.getLongPollServer" [("group_id" :: String) =: group_id] -- :: IO A.Value
    resToIO . eitherToRes . (`A.parseEither` longpoll) . A.withObject "" $ \obj -> do
        key    <- obj .: "key"    :: A.Parser String -- (string) — ключ;
        server <- obj .: "server" :: A.Parser String -- (string) — url сервера;
        ts     <- obj .: "ts"     :: A.Parser Int -- (string) — timestamp.
        return (key, server, ts)

withHandle :: Logger -> BotOptions -> Manager -> (Bot.Bot (Int, Int) -> IO ()) -> IO ()
withHandle log options mgr f = do
    stuff@(key, server, new_ts) <- getServer
    stuff <- newMVar stuff
    log Info $ "recieved a server and a key | " <> pack server
    let vkpoll serv = runVk log mgr token proxy serv parseUpdate ""
    f $ Bot.Bot
        { Bot.apiSendMessage = \(chat_id, _) -> sendMessage vkpre log chat_id
        , Bot.apiGetMessages = getMessages vkpoll log stuff getServer
        }
  where --vkpre :: (A.FromJSON c) => Text -> hz -> IO c
        vkpre = runVk log mgr token proxy "https://api.vk.com/method/" parseResult
        proxy = botProxy options
        token = maybe (error "No vkToken found") id (vkToken options)
        group_id = maybe (error "No vkGroupId found") show (vkGroupId options)
        getServer = getLongPollServer log vkpre group_id

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
    let req = request
    res <- httpLbs req mgr
    let resbody = responseBody res

    -- log Debug $ "recieved " <> (toStrict . LE.decodeUtf8 $ A.encode body)
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
                   ts <- obj .: "ts"
                   return (RenewTs ts)
               Just _ -> do
                   return PollFail
               Nothing -> do
                   ts <- obj .:  "ts" :: A.Parser Int
                   updates <- obj .: "updates"
                   return $ VkOk ts updates

getMessages vkpoll log stuff getServer = modifyMVar stuff getMessages'
    where getMessages' (key, server, ts) = do
        -- {$server}?act=a_check&key={$key}&ts={$ts}&wait=25
            updates <- vkpoll server
                [ "act" =: "a_check"
                , "key" =: key
                , "ts"  =: show ts
             -- , "wait" =: "25"
                ]
            case updates of
              VkOk new_ts updates ->
                  --let msgs = map (fmap toBotMsg . combMessages) updates
                  let msgs = mapMaybe (\u -> combMessages u >>= toBotMsg) updates
                  -- let msgs = catMaybes . map uniqueMsg . startFrom upd_offset $ updates
                      in return ((key, server, new_ts), msgs)
              RenewTs new_ts -> getMessages' (key, server, new_ts)
              PollFail -> getServer >>= getMessages'
          combMessages UnsupportedUpdate = Nothing
          combMessages (NewMessage msg) = Just msg
          toBotMsg msg = do
              txt <- Message._body msg
              return (Message._user_id msg, txt)

    {-
    let msgs = catMaybes . map uniqueMsg . startFrom ts $ updates
    let new_ts = if null updates then ts
                 else (Update._update_id . last $ updates)
    log Debug (pack $ "ts = " ++ show new_ts)
    writeFile file (show new_ts)
    return (new_ts, msgs)
    -}

sendMessage tgpre chatId text btns = undefined
