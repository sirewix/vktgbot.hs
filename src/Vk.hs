{-# LANGUAGE OverloadedStrings #-}

module Vk
    ( withHandle
    ) where
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson((.:),(.:?),(.=))
import Data.Text(Text,pack,unpack)
import GHC.Generics
import Misc
import Network.HTTP.Req
import Options
import Result
import SerDe
import qualified Bot              as Bot
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import qualified Logger           as Logger

withHandle :: Logger.Handle -> BotOptions -> (Bot.Handle (Int, Int) -> IO ()) -> IO ()
withHandle logger options f = do
    let file = "/tmp/vkbotupd"
    contents <- readFile file `catch` const (return "") :: IO Text
        -- \e -> const (return "") (e :: IOException)
    let offset = maybe 0 id (readT contents) :: Int
    ts <- newMVar offset
    -- print ("read offset from file = " ++ show offset)
    longpoll <- vkpre ("groups.getLongPollServer" :: Text)
                      ("group_id" =: (group_id :: Text))
                        :: IO A.Value
    (key, server, new_ts) <- resToIO . eitherToRes . A.parseEither . A.withObject "" $ \obj -> do
        key    <- obj .: "key"    -- (string) — ключ;
        server <- obj .: "server" -- (string) — url сервера;
        ts     <- obj .: "ts"     -- (string) — timestamp.

    Logger.logInfo logger "recieved a server and a key"
    -- {$server}?act=a_check&key={$key}&ts={$ts}&wait=25
    print longpoll
    print "HOIOH"
        {- seq (length contents) $ f $ Bot.Handle
        { Bot.apiSendMessage = \(chat_id, _) -> sendMessage vkpre logger chat_id
        , Bot.apiGetMessages = getMessages vkpre logger ts file
        } -}
  where --vkpre :: (A.FromJSON c) => Text -> hz -> IO c
        vkpre = runVk token
        token = maybe (error "No vkToken found") id (vkToken options)
        group_id = maybe (error "No vkGroupId found") (pack . show) (vkGroupId options)

runVk token ep query = (resToIO . parseResult) =<< (runReq defaultHttpConfig $ do
    {- let a = foldr (:) body
                [ "access_token" .= token
                , "v" .= ("5.110" :: Text)
                ]
    liftIO $ print a -}
    r <- req
        GET
        (https "api.vk.com" /: "method" /~ ep)
        --(ReqBodyJson a)
        NoReqBody
        jsonResponse
        --mempty
        (  ("access_token" =: token)
        <> ("v" =: ("5.110" :: Text))
        <> query
        )

    return (responseBody r :: A.Value))

parseResult :: (A.FromJSON a) => A.Value -> Result a
parseResult = eitherToRes . A.parseEither parser
    where parser = A.withObject "" $ \obj -> do
             res <- obj .:?  "response"
             case res of
               Just x -> return x
               Nothing -> do
                   err <- obj .: "error"
                   code <- err .: "error_code" :: A.Parser Int
                   msg <- err .: "error_msg"
                   fail $ "request failed with code " ++ show code ++ ": " ++ msg
          parser :: (A.FromJSON a) => A.Value -> A.Parser a

getMessages vkpre logger ts file = do
    modifyMVar ts $ \ts -> do
        -- useURI :: URI -> Maybe (Either (Url Http, Option scheme0) (Url Https, Option scheme1))
        --useURI ()
        updates <- runReq defaultHttpConfig $
            req
                GET

        let msgs = catMaybes . map uniqueMsg . startFrom ts $ updates
        let new_ts = if null updates then ts
                         else (Update._update_id . last $ updates)
        Logger.logDebug logger (pack $ "ts = " ++ show new_ts)
        writeFile file (show new_ts)
        return (new_ts, msgs)
