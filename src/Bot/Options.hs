{-# LANGUAGE
    OverloadedStrings
  #-}
module Bot.Options where

import           Logger                         ( Priority(..) )
import           Options                        ( Opt
                                                , lookupMod
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Misc                           ( int
                                                , parseEither
                                                , readT
                                                )
import           Text.Parsec                    ( (<|>)
                                                , alphaNum
                                                , char
                                                , eof
                                                , many1
                                                , oneOf
                                                , spaces
                                                )
import qualified Network.HTTP.Client           as HTTP
import qualified Network.HTTP.Client.TLS       as HTTP
import qualified Data.ByteString.Char8         as B

data BotOptions = BotOptions
    { logLevel :: Priority
    , updateDelay :: Int
    , managerSettings :: HTTP.ManagerSettings
    }

mkBotOptions :: String -> [Opt] -> Either Text BotOptions
mkBotOptions mod opts = do
  proxy  <- maybe (Right Nothing) (fmap Just <$> toProxy) (lookupMod mod "proxy" opts)
  loglvl <- opt "logLevel" Warning
  delay  <- opt "delay" 3000000
  return BotOptions
    { logLevel        = loglvl
    , updateDelay     = delay
    , managerSettings = HTTP.managerSetProxy (HTTP.proxyEnvironment proxy) HTTP.tlsManagerSettings
    }
 where
  opt k def =
    maybe (Left $ "Bad parameter " <> pack k) Right $ maybe (Just def) readT (lookupMod mod k opts)

toProxy :: Text -> Either Text HTTP.Proxy
toProxy = parseEither "proxy" $ do
  spaces
  host <- many1 $ alphaNum <|> oneOf "_."
  _    <- char ':'
  port <- int
  spaces
  eof
  return $ HTTP.Proxy { HTTP.proxyHost = B.pack host, HTTP.proxyPort = port }

