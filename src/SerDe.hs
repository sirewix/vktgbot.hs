module SerDe
  ( customParseOptions
  , enumParseOptions
  )
where

import           Data.Aeson
import           Data.Char

prefix = "_"
customParseOptions = defaultOptions { fieldLabelModifier = drop (length prefix) }

enumParseOptions =
  defaultOptions { constructorTagModifier = \(x : xs) -> toLower x : xs }
