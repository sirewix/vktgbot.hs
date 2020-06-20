module Session
  ( newSession
  , updateSession
  , Storage
  ) where


import Control.Concurrent
import Data.Hashable
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T

newSession
    :: (Eq sesid, Hashable sesid)
    => IO (Storage sesid state)
newSession =
    newMVar Map.empty

type Storage sesid state = MVar (Map.HashMap sesid state)

updateSession
    :: (Eq sesid, Hashable sesid)
    => IO state
    -> Storage sesid state
    -> sesid
    -> (state -> IO state)
    -> IO ()
updateSession defState db sesid f = do
    all <- readMVar db
    state <- case Map.lookup sesid all of
               Just state -> return state
               Nothing -> defState
    newState <- f state
    all <- takeMVar db
    putMVar db $ Map.insert sesid newState all

