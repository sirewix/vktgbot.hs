{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}

module Vk.Poll where

import           Control.Arrow                  ( left )
import           Control.Monad.Except           ( ExceptT(..)
                                                , liftEither
                                                )
import           Vk.Update                      ( Update(..) )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Logger                         ( Logger )
import           Misc                           ( (=:) )
import           Text.Read                      ( readMaybe )
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A

data VkPollResult =
    VkOk Int [Update]
  | RenewTs Int
  | PollFail

parseUpdate :: A.Value -> Either Text VkPollResult
parseUpdate = left pack . A.parseEither parser
 where
  parser = A.withObject "" $ \obj -> do
    failed <- obj .:? "failed" :: A.Parser (Maybe Int)
    case failed of
      Just 1 -> do
        tss <- obj .: "ts"
        let (Just ts) = readMaybe tss :: Maybe Int
        return (RenewTs ts)
      Just _ -> do
        return PollFail
      Nothing -> do
        tss <- obj .: "ts"
        let (Just ts) = readMaybe tss :: Maybe Int
        updates <- obj .: "updates"
        return $ VkOk ts updates

getLongPollServer
  :: Logger
  -> (Text -> [(Text, Text)] -> ExceptT Text IO A.Value)
  -> Text
  -> ExceptT Text IO (Text, Text, Text)
getLongPollServer _log vkpre group_id = do
  longpoll <- vkpre "groups.getLongPollServer" ["group_id" =: group_id]
  liftEither . left pack . (`A.parseEither` longpoll) . A.withObject "" $ \obj -> do
    key    <- obj .: "key"    :: A.Parser Text
    server <- obj .: "server" :: A.Parser Text
    ts     <- obj .: "ts"     :: A.Parser Text -- timestamp
    return (key, server, ts)

