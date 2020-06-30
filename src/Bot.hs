{-# LANGUAGE
  AllowAmbiguousTypes
, BlockArguments
, ExistentialQuantification
, MultiParamTypeClasses
, OverloadedStrings
, RankNTypes
#-}


module Bot
  ( Bot(..)
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
import Data.Traversable
import Logger
import Options
import Network.HTTP.Client(Manager)
import qualified Storage

data Bot sesid = Bot
    { apiSendMessage :: !(sesid -> Message -> Maybe [Button] -> IO ())
    , apiGetMessages :: !(IO [(sesid, Message)])
    }

type Message = Text
type Button = Text

data BotState a = BotState
    { content :: a
    , action :: BotIO a ()
    }

defBotState program def_substate = BotState
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

instance Functor (BotUserInteraction a) where
    fmap f (ReadMessage g) = ReadMessage (f . g)
    fmap f (SendMessage str btns next) = SendMessage str btns (f next)
    fmap f (ModifyState g next) = ModifyState g (f next)
    fmap f (ReadState g) = ReadState (f . g)
    -- fmap f (IOAction io) = IOAction (f <$> io)

type BotIO a = Free (BotUserInteraction a )
type Storage sesid a = Storage.Storage sesid (BotState a)

newStorage :: (Eq sesid, Hashable sesid) => IO (Storage sesid a)
newStorage = Storage.newStorage
-- newStorage = Storage.newStorage :: IO (Storage.Storage sesid (BotState a))

interpret
    :: Bot k
    -> Logger
    -> k
    -> BotIO a ()
    -> BotState a
    -> Maybe Message
    -> IO (BotState a)
interpret bot log sesid program = interpret'
    where interpret' state msg =
            case action state of
              Free (ReadMessage f) -> do
                  case msg of
                    Just msg -> do
                        log Debug "ReadMessage"
                        interpret' (state { action = f msg }) Nothing
                    Nothing -> return state
              Free (SendMessage m btns n) -> do
                  log Debug "SendMessage"
                  apiSendMessage bot sesid m btns
                  let s = state { action = n }
                  interpret' s msg
              Free (ModifyState f n) -> do
                  log Debug "ModifyState"
                  let newState = f (content state)
                      s = BotState { content = newState, action = n }
                  interpret' s msg
              Free (ReadState f) -> do
                  log Debug "ReadState"
                  let s = state { action = f (content state) }
                  interpret' s msg
              Pure _ -> do
                  log Debug "Pure"
                  let s = state { action = program }
                  interpret' s msg

groupMsgs
    :: (Eq sesid)
    => [(sesid, Message)]
    -> [(sesid, [Message])]
groupMsgs ((sesid, msg):rest) = (sesid, msg : map snd this) : (groupMsgs notthis)
    where (this, notthis) = partition ((==) sesid . fst) rest
groupMsgs [] = []

runBot
    :: (Show k, Eq k, Hashable k)
    => (Logger -> BotOptions -> Manager -> (Bot k -> IO ()) -> IO ())
    -> Logger
    -> BotOptions
    -> Manager
    -> BotIO s ()
    -> Storage k s
    -> s
    -> IO ()
runBot withHandle log options mgr program db defState = do
    withHandle log options mgr $ \bot -> do
        log Info "starting bot"
        void . forever $ do
            threadDelay 3000000
            forkIO $ do
                log Debug "recieving updates"
                msgs <- apiGetMessages bot
                let len = length msgs
                log Info $ if len > 0 then
                                          ("got " <> (pack . show $ len)
                                              <> " new message" <> (if len == 1 then "" else "s"))
                                      else
                                          ("no new messages")
                for (groupMsgs msgs) $ void . forkIO . \(someid, msgs) ->
                    Storage.updateStorage (defBotState program defState) db someid $ \state -> foldl
                            (\state msg -> do
                                log Debug ("processing message \"" <> msg <> "\"")
                                state <- state
                                interpret bot log someid program state (Just msg))
                            (pure state)
                            msgs
                return ()
