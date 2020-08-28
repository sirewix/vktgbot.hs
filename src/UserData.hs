module UserData
  ( newUserData
  , updateUserData
  , UserData
  )
where

import           Control.Concurrent             ( MVar
                                                , newMVar
                                                , putMVar
                                                , readMVar
                                                , takeMVar
                                                )
import           Control.Monad.Except           ( ExceptT, liftIO )
import           Data.Hashable                  ( Hashable )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.HashMap.Strict           as Map

type UserData sesid state = MVar (Map.HashMap sesid state)

newUserData :: (Eq sesid, Hashable sesid) => IO (UserData sesid state)
newUserData = newMVar Map.empty

updateUserData
  :: (Eq sesid, Hashable sesid)
  => state
  -> UserData sesid state
  -> sesid
  -> (state -> ExceptT e IO state)
  -> ExceptT e IO ()
updateUserData defState db sesid f = do
  all <- liftIO $ readMVar db
  let state = fromMaybe defState $ Map.lookup sesid all
  newState <- f state
  all      <- liftIO $ takeMVar db
  liftIO $ putMVar db $ Map.insert sesid newState all

