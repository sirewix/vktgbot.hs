{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Bot
  ( Handle(..)
  , BotIO(..)
  , BotState(..)
  , BotUserInteraction(..)
  , Button
  , Message
  , modifyState
  , newStorage
  , readMessage
  , readState
  , runBot
  , sendMessage
  , sendWithKeyboard
  ) where

import Control.Applicative((<|>))
import Control.Concurrent
import Control.Monad(forever,void)
import Control.Monad.Free
import Data.Hashable
import Data.List
import Data.Text(Text,pack,unpack)
import Options
import qualified Logger    as Logger
import qualified Session   as Session

data Handle sesid = Handle
    { apiSendMessage :: !(sesid -> Message -> Maybe [Button] -> IO ())
    , apiGetMessages :: !(IO [(sesid, Message)])
    }

type Message = Text
type Button = Text

data BotState a = BotState
    { content :: a
    , action :: BotIO a ()
    }

defBotState
    :: BotIO state ()
    -> state
    -> IO (BotState state)
defBotState program def_substate = do
        return $ BotState
            { content = def_substate
            , action = program
            }

data BotUserInteraction a next =
    ReadMessage (Message -> next)
  | SendMessage Text (Maybe [Button]) next
  | ModifyState (a -> a) next
  | ReadState (a -> next)
--  | IOAction (IO (next))

readMessage :: BotIO s Message
readMessage = Free (ReadMessage (\str -> Pure str))

sendMessage :: Text -> BotIO s ()
sendMessage str = Free (SendMessage str Nothing (Pure ()))

sendWithKeyboard :: Text -> [Button] -> BotIO s ()
sendWithKeyboard str btns = Free (SendMessage str (Just btns) (Pure ()))

modifyState :: (s -> s) -> BotIO s ()
modifyState f = Free (ModifyState f (Pure ()))

readState :: BotIO s s
readState = Free (ReadState (\state -> Pure state))

--rawIO :: IO a -> BotIO s a
--rawIO io = Free (IOAction io)

instance Functor (BotUserInteraction a) where
    fmap f (ReadMessage g) = ReadMessage (f . g)
    fmap f (SendMessage str btns next) = SendMessage str btns (f next)
    fmap f (ModifyState g next) = ModifyState g (f next)
    fmap f (ReadState g) = ReadState (f . g)
    -- fmap f (IOAction io) = IOAction (f <$> io)

type BotIO a = Free (BotUserInteraction a )
type Storage sesid a = Session.Storage sesid (BotState a)

newStorage :: (Eq sesid, Hashable sesid) => IO (Storage sesid a)
newStorage = Session.newSession
-- newStorage = Session.newSession :: IO (Session.Storage sesid (BotState a))

interpret
    :: Handle k
    -> Logger.Handle
    -> k
    -> BotIO a ()
    -> BotState a
    -> Maybe Message
    -> IO (BotState a)
interpret bot logger sesid program state msg = do
    case action state of
      Free (ReadMessage f) -> do
          case msg of
            Just msg -> do
                Logger.logDebug logger "ReadMessage"
                interpret bot logger sesid program (state { action = f msg }) Nothing
            Nothing -> return state
      Free (SendMessage m btns n) -> do
          Logger.logDebug logger "SendMessage"
          apiSendMessage bot sesid m btns
          let s = state { action = n }
          interpret bot logger sesid program s msg
      Free (ModifyState f n) -> do
          Logger.logDebug logger "ModifyState"
          let newState = f (content state)
          let s = BotState { content = newState, action = n }
          interpret bot logger sesid program s msg
      Free (ReadState f) -> do
          Logger.logDebug logger "ReadState"
          let s = state { action = f (content state) }
          interpret bot logger sesid program s msg
      {- Free (IOAction n) -> do
          Logger.logDebug logger "IOAction"
          next <- n
          let s = state { action = next }
          interpret bot logger sesid program s msg -}
      Pure _ -> do
          Logger.logDebug logger "Pure"
          let s = state { action = program }
          interpret bot logger sesid program s msg

groupMsgs
    :: (Eq sesid)
    => [(sesid, Message)]
    -> [(sesid, [Message])]
groupMsgs ((sesid, msg):rest) = (sesid, msg : map snd this) : (groupMsgs notthis)
    where (this, notthis) = partition ((==) sesid . fst) rest
groupMsgs [] = []

runBot
    :: (Show k, Eq k, Hashable k)
    => (Logger.Handle -> BotOptions -> (Handle k -> IO ()) -> IO ())
    -> Logger.Handle
    -> BotOptions
    -> BotIO s ()
    -> Storage k s
    -> s
    -> IO ()
runBot withHandle logger options program db defState = do
    --bot <- newHandle options db
    withHandle logger options $ \bot -> do
        Logger.logInfo logger "starting bot"
        void $ forever $ do
            threadDelay 3000000
            updateThread bot db program defState logger

updateThread
    :: (Eq sesid, Hashable sesid)
    => Handle sesid
    -> Bot.Storage sesid state
    -> BotIO state ()
    -> state
    -> Logger.Handle
    -> IO ()
updateThread bot db program defState logger = do
    -- Logger.logDebug logger "recieving updates"
    msgs <- apiGetMessages bot
    let len = length msgs
    Logger.logInfo logger $ if len > 0 then
                                ("got " <> (pack . show . length $ msgs)
                                    <> " new message" <> (if len == 1 then "" else "s"))
                             else
                                ("no new messages")
    let grouped = groupMsgs msgs
    sequence $ map (void . forkIO . g) grouped
    return ()
  where g (someid, msgs) =
          Session.updateSession (defBotState program defState) db someid $ \state -> do
              foldl (f someid) (pure state) msgs -- :: state
        f someid state msg = do
            Logger.logDebug logger ("processing message \"" <> msg <> "\"")
            state <- state
            interpret bot logger someid program state (Just msg)

