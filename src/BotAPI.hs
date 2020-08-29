-- this module contains everything to write API bindings
module BotAPI
  ( BotAPI(..)
  , Button
  , Message
  )
where

import           BotIO                          ( Message
                                                , Button
                                                )
import           Control.Monad.Except           ( ExceptT )
import           Data.Text                      ( Text )

data BotAPI sesid = BotAPI
    { apiSendMessage :: !(sesid -> Message -> Maybe [Button] -> ExceptT Text IO ())
    , apiGetMessages :: !(ExceptT Text IO [(sesid, Message)])
    }

