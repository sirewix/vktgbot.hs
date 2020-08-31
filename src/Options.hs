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
import           Misc                           ( parseEither
                                                , quotedString
                                                )
import           System.Environment             ( getArgs
                                                , getEnvironment
                                                )
import           Text.Parsec                    ( (<|>)
                                                , alphaNum
                                                , anyChar
                                                , char
                                                , char
                                                , endOfLine
                                                , eof
                                                , lookAhead
                                                , many1
                                                , manyTill
                                                , oneOf
                                                , sepBy
                                                , skipMany
                                                , try
                                                )
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
  cs <-
    ExceptT
    $   left (pack . show)
    <$> (E.try (TIO.readFile filename) :: IO (Either E.IOException Text))
  liftEither (parseConfig cs)

parseConfig :: Text -> Either Text [Opt]
parseConfig = parseEither "config" $ do
  res <- flip sepBy endOfLine $ comment <|> keyValuePair <|> (spaces >> return Nothing)
  eof
  return (catMaybes res)
 where
  eol  = void endOfLine
  eolf = eol <|> eof
  til x = anyChar `manyTill` (try . lookAhead $ x)
  simpleString = (unwords . words) <$> til eolf
  spaces  = skipMany $ oneOf spchr
  spchr   = [' ', '\t']
  comment = do
    spaces
    _ <- char '#'
    _ <- til eolf
    return Nothing

  keyValuePair = do
    spaces
    key <- many1 $ alphaNum <|> oneOf ['_', '.']
    spaces
    _ <- char '='
    spaces
    val <- quotedString <|> simpleString
    spaces
    return $ Just (key, pack val)


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

