{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Options( BotOptions(..)
              , getOptions
              ) where

import Control.Monad.IO.Class
import Data.Data( constrFields
                , Data
                , dataTypeConstrs
                , dataTypeOf
                , showConstr
                , toConstr
                )
import Data.List(find)
import Data.Maybe
import Data.Text(Text,pack,unpack)
import Data.Typeable
import GHC.Generics
import Misc
import System.Environment
import System.IO
import Text.Parsec hiding (Ok)
import Text.Parsec.Char
import Text.Parsec.Text
import Text.Parsec.Error
import qualified Data.ByteString.Char8 as B
import qualified Logger                as Logger
import qualified Network.HTTP.Client   as HttpClient

import Result

data BotOptions = BotOptions
    { tgToken     :: Maybe String
    , botProxy     :: Maybe HttpClient.Proxy
    , repeatTimes :: Int
    , helpText    :: Text
    , logLevel    :: Logger.Priority
    , vkToken     :: Maybe String
    , vkGroupId   :: Maybe Int
    } deriving(Data, Typeable, Show, Generic)

deriving instance Data Logger.Priority
deriving instance Data HttpClient.Proxy

getOptions :: IO (BotOptions)
getOptions = do -- IO
    args <- getArgs
    env <- getEnvOpts
    --return (parseArgs args >>= (\cliArgs -> resolveOptions (cliArgs ++ env)))
    cliArgs <- resToIO $ parseArgs args
    --let (Ok cliArgs) = parseArgs args
    fromConf <- case (findOption "config" cliArgs) of
                    Nothing -> return ([] :: [Opt])
                    Just fname -> fromConfig fname
    resToIO $ resolveOptions (cliArgs ++ env ++ fromConf)

resolveOptions :: [Opt] -> Result BotOptions
resolveOptions opts = do
    _tgProxy <- maybe (Ok Nothing) (fmap Just <$> toProxy . pack)
                                             (option "botProxy")
    return $ BotOptions
        { tgToken =                           option "tgToken"
        , botProxy = _tgProxy
        , repeatTimes = maybe 5 id           (option "repeatTimes" >>= readT)
        , helpText = maybe defaultHelp pack  (option "helpText")
        , logLevel = maybe Logger.Warning id (option "logLevel" >>= readT)
        , vkToken =                           option "vkToken"
        , vkGroupId =                        (option "vkGroupId" >>= readT)
        }
    where required (Just opt) _ = return opt
          required Nothing msg = fail msg
          option opt = findOption opt opts
          defaultHelp = "I don't know who am I" :: Text

          toProxy :: Text -> Result (HttpClient.Proxy)
          toProxy = either (Err . ("Error parsing proxy: " ++) . showShort) (Ok) . parse addr ""
              where addr = do
                        spaces
                        host <- many1 $ alphaNum <|> oneOf "_."
                        char ':'
                        port <- int
                        spaces
                        eof
                        return $ HttpClient.Proxy
                            { HttpClient.proxyHost = B.pack host
                            , HttpClient.proxyPort = port }

int :: Parser Int
int = read <$> many1 digit

showShort :: ParseError -> String
showShort err =
    showErrorMessages "or" "unknown parse error"
                      "expecting" "unexpected" "end of input"
                      (errorMessages err)

fromConfig :: FilePath -> IO ([Opt])
fromConfig filename = catMaybes <$> to_io <$> parseFromFile config filename
    where
        to_io (Right a) =  a
        to_io (Left e) = error ("Config parsing error: " ++ show e)
        eol = endOfLine >> return ()
        eolf = eol <|> eof
        til x = anyChar `manyTill` (try . lookAhead $ x)
        sstring = do
            x <- til eolf
            return (unwords . words $ x) -- eeh

        qstring = between (string "\"\n" <|> string "\"") (string "\n\"" <|> string "\"") qchar
        qchar = many $ noneOf ['\\', '"'] <|> do
            char '\\'
            c <- oneOf ['\\', '"', 'n']
            return $ case c of
                       'n' -> '\n'
                       x -> x

        spaces = skipMany $ oneOf spchr
        spchr = [' ', '\t']
        comment = do
            spaces
            char '#'
            til eolf
            return Nothing

        definition = do
            spaces
            key <- many1 $ alphaNum <|> char '_'
            -- True <- pure $ key `elem` optionsList
            spaces
            char '='
            spaces
            val <- qstring <|> sstring
            spaces
            return . Just $ Opt ConfFile key val

        config = do
            res <- flip sepBy endOfLine $
                   comment
               <|> definition
               <|> (spaces >> return Nothing)
            eof
            return res

parseArgs :: [String] -> Result [Opt]
parseArgs args = sequence $ f args
    where f (('-':'-':arg):val:rest)
            | arg `elem` "config":optionsList = return ((Opt CliArg arg val)) : f rest
                                              -- = f rest >>= ((:) (Opt CliArg arg val))
            | otherwise = fail ("Unexpected command line argument \"" ++ arg ++ "\"")
          f (('-':'-':arg):rest) = fail ("Flag parameters are not allowed. Use \"" ++ arg ++ " true\" for that")
          f [] = []

getEnvOpts :: IO [Opt]
getEnvOpts = do
        env <- getEnvironment
        return (map toOpt $ filter f env)
    where
        f (key, _) = key `elem` optionsList
        toOpt (key, val) = Opt EnvVar key val

optionsList :: [String]
optionsList = concat $ map constrFields (dataTypeConstrs (dataTypeOf (undefined::BotOptions)))
data OptSource = CliArg | EnvVar | ConfFile
    deriving(Show)

data Opt = Opt OptSource String String
    deriving(Show)

findOption :: String -> [Opt] -> Maybe String
findOption key opts = extractValue <$> (find (eqOpt key) opts)
    where extractValue (Opt _ _ val) = val
          eqOpt key1 (Opt _ key2 _) = key1 == key2

