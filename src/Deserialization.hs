module Deserialization
  ( customParseOptions
  , enumParseOptions
  )
where

import           Data.Aeson                     ( defaultOptions
                                                , fieldLabelModifier
                                                , constructorTagModifier
                                                )
import           Data.Char                      ( toLower )

prefix = "_"
customParseOptions = defaultOptions { fieldLabelModifier = drop (length prefix) }

enumParseOptions =
  defaultOptions { constructorTagModifier = \(x : xs) -> toLower x : xs }
