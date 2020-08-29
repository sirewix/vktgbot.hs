{-# LANGUAGE FlexibleContexts #-}
module Misc
  ( (=:)
  , int
  , loggedExceptT
  , parseEither
  )
where

import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                , (<=<)
                                                )
import           Data.Text                      ( Text, pack )
import           Logger                         ( Logger
                                                , Priority(..)
                                                )
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

-- custom error printer to avoid printing line number,
-- constant strings were copied from parsec source code
showShort :: ParseError -> String
showShort err = showErrorMessages "or"
                                  "unknown parse error"
                                  "expecting"
                                  "unexpected"
                                  "end of input"
                                  (errorMessages err)

parseEither what parser =
  either (Left . pack . (("Error parsing " <> what <> ": ") <>) . showShort) Right
    . parse parser ""

(=:) :: a -> b -> (a, b)
a =: b = (a, b)

loggedExceptT :: Logger -> ExceptT Text IO () -> IO ()
loggedExceptT log = either (log Error) pure <=< runExceptT
