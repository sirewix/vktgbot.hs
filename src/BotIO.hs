-- this module contains everything to write bot logic
module BotIO where
import           Control.Monad.Free             ( Free(..) )
import           Data.Text                      ( Text )

data BotState a = BotState
    { content :: a
    , action :: BotIO a ()
    }

data BotUserInteraction a next =
    ReadMessage (Message -> next)
  | SendMessage Text (Maybe [Button]) next
  | ModifyState (a -> a) next
  | ReadState (a -> next)

instance Functor (BotUserInteraction a) where
  fmap f (ReadMessage g            ) = ReadMessage (f . g)
  fmap f (SendMessage str btns next) = SendMessage str btns (f next)
  fmap f (ModifyState g next       ) = ModifyState g (f next)
  fmap f (ReadState g              ) = ReadState (f . g)

type BotIO a = Free (BotUserInteraction a)

type Message = Text
type Button = Text

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

