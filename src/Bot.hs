{-# LANGUAGE
    AllowAmbiguousTypes
  , BlockArguments
  , ExistentialQuantification
  , MultiParamTypeClasses
  , OverloadedStrings
  , RankNTypes
  #-}

module Bot
  ( BotAPI(..)
  , BotIO
  , BotState(..)
  , BotUserInteraction(..)
  , Button
  , Message
  , groupMsgs
  , interpret
  , mkBotOptions
  , modifyState
  , newUserData
  , readMessage
  , readState
  , runBot
  , sendMessage
  , sendWithKeyboard
  )
where

import           Control.Concurrent             ( MVar
                                                , forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( forever
                                                , foldM
                                                , void
                                                )
import           Control.Monad.Free             ( Free(..) )
import           Control.Monad.Except           ( ExceptT
                                                , liftIO
                                                )
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
import           Misc                           ( int
                                                , loggedExceptT
                                                , parseEither
                                                )
import           Options                        ( Opt
                                                , lookupMod
                                                )
import           Text.Parsec                    ( (<|>)
                                                , alphaNum
                                                , char
                                                , eof
                                                , many1
                                                , oneOf
                                                , spaces
                                                )
import           Text.Read                      ( readMaybe )
import qualified Data.ByteString.Char8         as B
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Client.TLS       as HTTP
import qualified UserData
import qualified System.IO

data BotAPI sesid = BotAPI
    { apiSendMessage :: !(sesid -> Message -> Maybe [Button] -> ExceptT Text IO ())
    , apiGetMessages :: !(ExceptT Text IO [(sesid, Message)])
    }

type Message = Text
type Button = Text

data BotState a = BotState
    { content :: a
    , action :: BotIO a ()
    }

defBotState program def_substate = BotState { content = def_substate, action = program }

data BotUserInteraction a next =
    ReadMessage (Message -> next)
  | SendMessage Text (Maybe [Button]) next
  | ModifyState (a -> a) next
  | ReadState (a -> next)

readMessage :: BotIO s Message
readMessage = Free (ReadMessage Pure)

sendMessage :: Text -> BotIO s ()
sendMessage str = Free (SendMessage str Nothing (Pure ()))

sendWithKeyboard :: Text -> [Button] -> BotIO s ()
sendWithKeyboard str btns = Free (SendMessage str (Just btns) (Pure ()))

modifyState :: (s -> s) -> BotIO s ()
modifyState f = Free (ModifyState f (Pure ()))

readState :: BotIO s s
readState = Free (ReadState Pure)

instance Functor (BotUserInteraction a) where
  fmap f (ReadMessage g            ) = ReadMessage (f . g)
  fmap f (SendMessage str btns next) = SendMessage str btns (f next)
  fmap f (ModifyState g next       ) = ModifyState g (f next)
  fmap f (ReadState g              ) = ReadState (f . g)

type BotIO a = Free (BotUserInteraction a)
type UserData sesid a = UserData.UserData sesid (BotState a)

newUserData :: (Eq sesid, Hashable sesid) => IO (UserData sesid a)
newUserData = UserData.newUserData

interpret
  :: BotAPI k -> Logger -> k -> BotIO a () -> BotState a -> Maybe Message -> ExceptT Text IO (BotState a)
interpret bot log sesid program = interpret'
 where
  interpret' state msg = case action state of
    Free (ReadMessage f) -> do
      case msg of
        Just msg -> do
          liftIO $ log Debug "ReadMessage"
          interpret' (state { action = f msg }) Nothing
        Nothing -> return state
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
    Pure _ -> do
      liftIO $ log Debug "Pure"
      return $ state { action = program }

groupMsgs :: (Eq sesid) => [(sesid, msg)] -> [(sesid, [msg])]
groupMsgs ((sesid, msg) : rest) = (sesid, msg : map snd this) : groupMsgs notthis
  where (this, notthis) = partition ((==) sesid . fst) rest
groupMsgs [] = []


data BotOptions = BotOptions
    { logLevel :: Priority
    , updateDelay :: Int
    , managerSettings :: HTTP.ManagerSettings
    }

mkBotOptions :: String -> [Opt] -> Either Text BotOptions
mkBotOptions mod opts = do
  proxy  <- maybe (Right Nothing) (fmap Just <$> toProxy . pack) (lookupMod mod "proxy" opts)
  loglvl <- opt "logLevel" Warning
  delay  <- opt "delay" 3000000
  return BotOptions
    { logLevel        = loglvl
    , updateDelay     = delay
    , managerSettings = HTTP.managerSetProxy (HTTP.proxyEnvironment proxy) HTTP.tlsManagerSettings
    }
 where
  opt k def =
    maybe (Left $ "Unexpected " <> pack k) Right $ maybe (Just def) readMaybe (lookupMod mod k opts)

toProxy :: Text -> Either Text HTTP.Proxy
toProxy = parseEither "proxy" $ do
  spaces
  host <- many1 $ alphaNum <|> oneOf "_."
  _    <- char ':'
  port <- int
  spaces
  eof
  return $ HTTP.Proxy { HTTP.proxyHost = B.pack host, HTTP.proxyPort = port }

runBot
  :: (Show k, Eq k, Hashable k)
  => BotIO s ()
  -> s
  -> MVar System.IO.Handle
  -> Text
  -> (Logger -> HTTP.Manager -> ExceptT Text IO (BotAPI k))
  -> BotOptions
  -> IO ()
runBot program defState logOutput prefix newHandle options = do
  mgr <- HTTP.newManager (managerSettings options)
  db  <- Bot.newUserData
  log Info "starting bot"
  loggedExceptT log $ do
    bot <- newHandle log mgr
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

processMessages log bot db program defState (someid, msgs) = loggedExceptT log $
  UserData.updateUserData (defBotState program defState) db someid $ \state -> foldM
    (\state msg -> do
      liftIO $ log Debug ("processing message \"" <> msg <> "\"")
      interpret bot log someid program state (Just msg)
    )
    state
    msgs
