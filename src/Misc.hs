{-# LANGUAGE FlexibleContexts #-}
module Misc
  ( readT
  , int
  , parseToRes
  , (=:)
  )
where

import           Data.Maybe                     ( listToMaybe )
import           Result
import           Text.Parsec             hiding ( Ok )
import           Text.Parsec.Error
import           Text.Parsec.Text

readT :: (Read a) => String -> Maybe a
readT str = fst <$> listToMaybe (reads str)

int :: Parser Int
int = read <$> many1 digit

showShort :: ParseError -> String
showShort err = -- copied from parsec source code
                showErrorMessages "or"
                                  "unknown parse error"
                                  "expecting"
                                  "unexpected"
                                  "end of input"
                                  (errorMessages err)

parseToRes what parser =
  either (Err . (("Error parsing " <> what <> ": ") <>) . showShort) Ok
    . parse parser ""

(=:) :: a -> b -> (a, b)
a =: b = (a, b)

