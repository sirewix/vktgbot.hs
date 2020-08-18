{-# LANGUAGE FlexibleContexts #-}
module Misc
  ( int
  , parseToRes
  , (=:)
  )
where

import           Result                         ( Result(..) )
import           Text.Parsec                    ( many1
                                                , digit
                                                , parse
                                                )
import           Text.Parsec.Error              ( ParseError
                                                , showErrorMessages
                                                , errorMessages
                                                )
import           Text.Parsec.Text               ( Parser )

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

