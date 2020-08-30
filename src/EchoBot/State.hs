{-# LANGUAGE OverloadedStrings #-}
module EchoBot.State where

import           Data.Text                      ( Text, pack )
import           Options                        ( Opt )
import           Misc                           ( parseEither
                                                , int
                                                )

newtype EchoBotState = EchoBotState
    { nrepeat :: Int }
    deriving (Eq, Show)

defaultState :: [Opt] -> Either Text EchoBotState
defaultState opts = EchoBotState <$> opt "repeatTimes" 5
 where
  opt k def =
    maybe (Right def) (parseEither ("parameter " <> pack k) int) (lookup k opts)
