module Deserialization
  ( customParseOptions
  , enumParseOptions
  )
where

import           Data.Aeson                     ( Options
                                                , defaultOptions
                                                , fieldLabelModifier
                                                , constructorTagModifier
                                                )
import           Data.Char                      ( toLower )

customParseOptions :: Options
customParseOptions = defaultOptions { fieldLabelModifier = drop (length prefix) }
    where prefix = "_"

enumParseOptions :: Options
enumParseOptions =
  defaultOptions { constructorTagModifier = \(x : xs) -> toLower x : xs }
