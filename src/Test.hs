{-# LANGUAGE
  DeriveFunctor
, FlexibleContexts
, FlexibleInstances
, GeneralizedNewtypeDeriving
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, OverloadedStrings
, AllowAmbiguousTypes
, ScopedTypeVariables
, TypeApplications
, PartialTypeSignatures
, KindSignatures
#-}
-- , FunctionalDependencies
module Test where

import Control.Concurrent
import Control.Monad.Reader
import Data.Hashable
import Data.Text
import Data.Text.IO(hPutStrLn)
import Logger
import Options
import qualified Data.HashMap.Strict as Map
import qualified System.IO           as System.IO

class FromOpts m where
    makeEnv :: BotOptions -> IO m

type HLogger = (Priority, MVar System.IO.Handle)

hlogger :: Priority -> Text -> HLogger -> IO ()
hlogger prio msg (loglvl, h) =
    if prio >= loglvl then
        withMVar h $ \fileH ->
            hPutStrLn fileH $ (pack $ '[' : show prio ++ "] ")  <>  msg
    else return ()

instance FromOpts HLogger where
    makeEnv opts = do
        h <- newMVar System.IO.stdout
        return (logLevel opts, h)

class FromOpts m
  => Module e m where
  -- => Module e m | e -> m where
    getEnv :: e -> m

class (FromOpts m, Module e m)
  => Logger e m where
    logg :: Priority -> Text -> App e ()

data Env s = Env
    { getLogger :: !HLogger
    , tgStorage :: !(TgStorage s)
    }

instance FromOpts (Env s) where
    makeEnv opts = do
        logenv <- makeEnv opts
        tgstorage <- makeEnv opts
        return $ Env
            { getLogger = logenv
            , tgStorage = tgstorage
            }

instance Module (Env s) HLogger where
    getEnv = getLogger

instance Module (Env s) (TgStorage s) where
    getEnv = tgStorage

instance FromOpts (TgStorage s) where
    -- makeEnv = return . TgStorage . makeEnv
    -- makeEnv :: BotOptions -> IO m
    makeEnv opts = TgStorage <$> makeEnv opts

instance Logger (Env s) HLogger where
    logg prio msg = localToGlobalEnv $ hlogger prio msg

newtype TgStorage s = TgStorage (RamStorage (Int, Int) s)

newtype App e a = App { unApp :: ReaderT e IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

runApp :: App e a -> e -> IO a
runApp app opts = runReaderT (unApp app) opts

localToGlobalEnv :: (Module e m) => (m -> IO a) -> App e a
localToGlobalEnv f = App . ReaderT $ f . getEnv

type RamStorage sesid state = MVar (Map.HashMap sesid state)

instance FromOpts (RamStorage sesid state) where
    makeEnv opts = newMVar Map.empty

class (Eq sesid, Hashable sesid, Module e m)
  => Storage sesid state e m where
    updateSession
        :: state
        -> sesid
        -> (state -> App e state)
        -> App e ()

-- updateSession defState sesid f db = do

    {-
instance (Eq sesid, Hashable sesid)
  => Storage sesid state (Env state)
    newStorage _ = newMVar Map.empty
    updateSession defState sesid f db = liftIO $ do
        all <- readMVar db
        state <- case Map.lookup sesid all of
                   Just state -> return state
                   Nothing -> defState
        newState <- f state
        all <- takeMVar db
        putMVar db $ Map.insert sesid newState all
    -}

kindamain :: IO ()
kindamain = do
    opts <- getOptions
    env <- makeEnv opts :: IO (Env Int)
    --let prog = proga :: (Logger (Env Int) HLogger) => App (Env Int) ()
    let prog = proga @(Env Int) @HLogger -- :: (Logger (Env Int) HLogger) => App (Env Int) ()
    runApp prog (env)

proga :: forall e m. (Logger e m) => App e ()
proga = do
    logg @_ @m Error "msg"
    logg Error "msg"
