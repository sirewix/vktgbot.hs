{-# LANGUAGE OverloadedStrings #-}
module EchoBot.Options where

import           Data.Text                      ( Text )
import           Data.Maybe                     ( fromMaybe )
import           Options                        ( Opt )

newtype EchoBotOptions = EchoBotOptions
    { helpText :: Text }

mkEchoBotOptions :: [Opt] -> Either Text EchoBotOptions
mkEchoBotOptions opts = pure
  $ EchoBotOptions { helpText = fromMaybe helptxt $ lookup "helpText" opts }
 where
  helptxt =
    "Hi, this is simple echo bot" <> "/help — this message" <> "/repeat — set repeat"


