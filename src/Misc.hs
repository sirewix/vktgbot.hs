{-# LANGUAGE
    OverloadedStrings
  , FlexibleContexts
  #-}
module Misc
  ( (=:)
  , int
  , loggedExceptT
  , parseEither
  , readT
  , quotedString
  )
where

import           Control.Arrow                  ( left )
import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                , (<=<)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Logger                         ( Logger
                                                , Priority(..)
                                                )
import           Text.Parsec                    ( (<|>)
                                                , between
                                                , char
                                                , digit
                                                , many
                                                , many1
                                                , noneOf
                                                , oneOf
                                                , parse
                                                , string
                                                )
import           Text.Parsec.Error              ( ParseError
                                                , showErrorMessages
                                                , errorMessages
                                                )
import           Text.Parsec.Text               ( Parser )
import           Text.Read                      ( readMaybe )

-- custom error printer to avoid printing line number,
-- constant strings were copied from parsec source code
showShort :: ParseError -> Text
showShort err = pack $ showErrorMessages
    "or"
    "unknown parse error"
    "expecting"
    "unexpected"
    "end of input"
    (errorMessages err)

parseEither :: Text -> Parser a -> Text -> Either Text a
parseEither what parser =
  left ((("Error parsing " <> what <> ": ") <>) . showShort)
    . parse parser ""

(=:) :: a -> b -> (a, b)
a =: b = (a, b)

loggedExceptT :: Logger -> ExceptT Text IO () -> IO ()
loggedExceptT log = either (log Error) pure <=< runExceptT

readT :: Read a => Text -> Maybe a
readT = readMaybe . unpack

quotedString :: Parser String
quotedString = between (string "\"" <|> string "\"\n")
                       (string "\"" <|> string "\n\"")
                       (many qchar)
 where
  qchar = noneOf ['\\', '"'] <|> do
    _ <- char '\\'
    c <- oneOf ['\\', '"', 'n']
    return $ case c of
      'n' -> '\n'
      x   -> x

int :: Parser Int
int = read <$> many1 digit

