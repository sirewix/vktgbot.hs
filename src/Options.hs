{-# LANGUAGE
  DeriveDataTypeable
, DeriveGeneric
, StandaloneDeriving
, OverloadedStrings
, FlexibleContexts
#-}

module Options( Opt
              , getOptions
              , lookupMod
              ) where

import Control.Monad.IO.Class
import Data.Data( constrFields
                , Data
                , dataTypeConstrs
                , dataTypeOf
                , showConstr
                , toConstr
                )
import Data.List(find,isPrefixOf)
import Data.Maybe
import Data.Text(Text,pack,unpack)
import Data.Typeable
import GHC.Generics
import Misc
import qualified Control.Applicative as A
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

deriving instance Data Logger.Priority
deriving instance Data HttpClient.Proxy

getOptions :: IO [Opt]
getOptions = do -- IO
    args <- getArgs
    env <- getEnvOpts
    cliArgs <- resToIO $ parseArgs args
    fromConf <- case (lookup "config" cliArgs) of
                    Nothing -> return ([] :: [Opt])
                    Just fname -> fromConfig fname
    return (cliArgs ++ env ++ fromConf)

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
            key <- many1 $ alphaNum <|> oneOf ['_', '.']
            -- True <- pure $ key `elem` optionsList
            spaces
            char '='
            spaces
            val <- qstring <|> sstring
            spaces
            return $ Just (key, val)

        config = do
            res <- flip sepBy endOfLine $
                   comment
               <|> definition
               <|> (spaces >> return Nothing)
            eof
            return res

parseArgs :: [String] -> Result [Opt]
parseArgs args = sequence $ f args
    where f (('-':'-':arg):val:rest) = return (arg, val) : f rest
          f (('-':'-':arg):rest) = fail ("Flag parameters are not allowed. Use \"" ++ arg ++ " true\" for that")
          f [] = []

getEnvOpts :: IO [Opt]
getEnvOpts = do
        env <- getEnvironment
        return (map toOpt . filter f $ env)
    where
        f (key, _) = prefix `isPrefixOf` key
        prefix = "BOT_"
        toOpt (key, val) = (drop (length prefix) (map fromEnv key), val)
        fromEnv '_' = '.'
        fromEnv c = c

type Opt = (String, String)

lookupMod mod key opts =
          lookup (mod <> "." <> key) opts
    A.<|> lookup key opts

