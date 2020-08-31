{-# LANGUAGE
    AllowAmbiguousTypes
  , BlockArguments
  , ExistentialQuantification
  , MultiParamTypeClasses
  , OverloadedStrings
  , RankNTypes
  #-}

module Bot
  ( groupMsgs
  , interpret
  , runBot
  )
where

import           Bot.API                        ( BotAPI(..) )
import           Bot.IO                         ( BotIO
                                                , BotState(..)
                                                , BotUserInteraction(..)
                                                , Message
                                                )
import           Bot.Options                    ( BotOptions(..) )
import           Control.Concurrent             ( MVar
                                                , forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( forever
                                                , foldM
                                                , void
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , liftIO
                                                )
import           Control.Monad.Free             ( Free(..) )
import           Data.Foldable                  ( for_ )
import           Data.Hashable                  ( Hashable )
import           Data.List                      ( partition )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Logger                         ( Logger
                                                , Priority(..)
                                                , sublog
                                                , newLogger
                                                )
import           Misc                           ( loggedExceptT )
import           UserData                       ( UserData
                                                , newUserData
                                                , updateUserData
                                                )
import qualified Network.HTTP.Client           as HTTP
import qualified System.IO

interpret
  :: BotAPI k
  -> Logger
  -> k
  -> BotIO s a
  -> BotState s a
  -> Maybe Message
  -> ExceptT Text IO (Maybe a, BotState s a)
interpret bot log sesid program = interpret'
 where
  interpret' state msg = case action state of
    Free (ReadMessage f) -> do
      case msg of
        Just msg -> do
          liftIO $ log Debug "ReadMessage"
          interpret' (state { action = f msg }) Nothing
        Nothing -> return (Nothing, state)
    Free (SendMessage m btns n) -> do
      liftIO $ log Debug "SendMessage"
      apiSendMessage bot sesid m btns
      let s = state { action = n }
      interpret' s msg
    Free (ModifyState f n) -> do
      liftIO $ log Debug "ModifyState"
      let newState = f (content state)
          s        = BotState { content = newState, action = n }
      interpret' s msg
    Free (ReadState f) -> do
      liftIO $ log Debug "ReadState"
      let s = state { action = f (content state) }
      interpret' s msg
    Pure a -> do
      liftIO $ log Debug "Pure"
      return (Just a, state { action = program })

groupMsgs :: (Eq sesid) => [(sesid, msg)] -> [(sesid, [msg])]
groupMsgs ((sesid, msg) : rest) = (sesid, msg : map snd this) : groupMsgs notthis
  where (this, notthis) = partition ((==) sesid . fst) rest
groupMsgs [] = []

runBot
  :: (Show k, Eq k, Hashable k)
  => BotIO s ()
  -> s
  -> MVar System.IO.Handle
  -> Text
  -> (Logger -> HTTP.Manager -> ExceptT Text IO (BotAPI k))
  -> BotOptions
  -> IO ()
runBot program defState logOutput prefix newAPI options = do
  mgr <- HTTP.newManager (managerSettings options)
  db  <- newUserData
  log Info "starting bot"
  loggedExceptT log $ do
    bot <- newAPI log mgr
    liftIO $ void . forever . loggedExceptT log $ do
      liftIO $ log Debug "recieving updates"
      msgs <- apiGetMessages bot
      let len = length msgs
      liftIO $ log Info $ if len > 0
        then "got "
          <> (pack . show $ len)
          <> " new message"
          <> (if len == 1 then "" else "s")
        else "no new messages"
      liftIO $ for_ (groupMsgs msgs) $ void . forkIO . processMessages log bot db program defState
      liftIO $ threadDelay (updateDelay options)
  where log = sublog prefix $ newLogger logOutput (logLevel options)

processMessages
  :: (Eq k, Hashable k)
  => Logger
  -> BotAPI k
  -> UserData k (BotState s ())
  -> BotIO s ()
  -> s
  -> (k, [Message])
  -> IO ()
processMessages log bot db program defState (someid, msgs) = loggedExceptT log $
  updateUserData defBotState db someid $ \state -> foldM
    (\state msg -> do
      liftIO $ log Debug ("processing message \"" <> msg <> "\"")
      snd <$> interpret bot log someid program state (Just msg)
    )
    state
    msgs
  where defBotState = BotState { content = defState, action = program }
