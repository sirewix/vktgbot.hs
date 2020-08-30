{-# LANGUAGE
    DeriveDataTypeable
  , DeriveGeneric
  , StandaloneDeriving
  , OverloadedStrings
  , FlexibleContexts
  #-}

module Options
  ( Opt
  , getOptions
  , lookupMod
  )
where

import           Control.Arrow                  ( left )
import           Control.Monad.Except           ( ExceptT(..)
                                                , liftIO
                                                , liftEither
                                                , void
                                                )
import           Data.List                      ( isPrefixOf )
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Misc                           ( parseEither )
import           System.Environment             ( getArgs
                                                , getEnvironment
                                                )
import           Text.Parsec                    ( (<|>)
                                                , alphaNum
                                                , anyChar
                                                , between
                                                , char
                                                , char
                                                , endOfLine
                                                , eof
                                                , lookAhead
                                                , many
                                                , many1
                                                , manyTill
                                                , noneOf
                                                , oneOf
                                                , sepBy
                                                , skipMany
                                                , string
                                                , try
                                                )
import           Text.Parsec.Text               ( Parser )
import qualified Control.Applicative           as A
import qualified Control.Exception             as E
import qualified Data.Text.IO                  as TIO

type Opt = (String, Text)

lookupMod :: String -> String -> [Opt] -> Maybe Text
lookupMod mod key opts = lookup (mod <> "." <> key) opts A.<|> lookup key opts

getOptions :: ExceptT Text IO [Opt]
getOptions = do
  cliArgs  <- liftIO getArgs >>= liftEither . parseArgs
  envArgs  <- liftIO getEnvOpts
  fromConf <- maybe (pure []) fromConfig (unpack <$> lookup "config" cliArgs)
  return (cliArgs ++ envArgs ++ fromConf)

fromConfig :: FilePath -> ExceptT Text IO [Opt]
fromConfig filename = do
    cs <- ExceptT $ left (pack . show) <$> (E.try (TIO.readFile filename) :: IO (Either E.IOException Text))
    liftEither $ parseEither "config" config cs
 where
  eol  = void endOfLine
  eolf = eol <|> eof
  til x = anyChar `manyTill` (try . lookAhead $ x)
  sstring = do
    x <- til eolf
    return (unwords . words $ x) -- eeh

  qstring = between (string "\"\n" <|> string "\"") (string "\n\"" <|> string "\"") (many qchar)
  qchar   = noneOf ['\\', '"'] <|> do
    _ <- char '\\'
    c <- oneOf ['\\', '"', 'n']
    return $ case c of
      'n' -> '\n'
      x   -> x

  spaces  = skipMany $ oneOf spchr
  spchr   = [' ', '\t']
  comment = do
    spaces
    _ <- char '#'
    _ <- til eolf
    return Nothing

  definition = do
    spaces
    key <- many1 $ alphaNum <|> oneOf ['_', '.']
    spaces
    _ <- char '='
    spaces
    val <- qstring <|> sstring
    spaces
    return $ Just (key, pack val)

  config :: Parser [Opt]
  config = do
    res <- flip sepBy endOfLine $ comment <|> definition <|> (spaces >> return Nothing)
    eof
    return (catMaybes res)

parseArgs :: [String] -> Either Text [Opt]
parseArgs = sequence . f
 where
  f (('-' : '-' : arg) : val : rest) = return (arg, pack val) : f rest
  f (('-' : '-' : arg) : _) =
    fail ("Flag parameters are not allowed. Use \"--" ++ arg ++ " true\" for that")
  f (val : _) = fail ("Unexpected CLI argument \"" ++ val ++ "\"")
  f []        = []

getEnvOpts :: IO [Opt]
getEnvOpts = map toOpt . filter f <$> getEnvironment
 where
  f (key, _) = prefix `isPrefixOf` key
  prefix = "BOT_"
  toOpt (key, val) = (drop (length prefix) (map fromEnv key), pack val)
  fromEnv '_' = '.'
  fromEnv c   = c

