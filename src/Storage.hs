module Storage
  ( newStorage
  , updateStorage
  , Storage
  ) where


import Control.Concurrent
import Data.Hashable
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T

type Storage sesid state = MVar (Map.HashMap sesid state)

newStorage
    :: (Eq sesid, Hashable sesid)
    => IO (Storage sesid state)
newStorage = newMVar Map.empty

updateStorage
    :: (Eq sesid, Hashable sesid)
    => state
    -> Storage sesid state
    -> sesid
    -> (state -> IO state)
    -> IO ()
updateStorage defState db sesid f = do
    all <- readMVar db
    let state = maybe defState id $ Map.lookup sesid all
    newState <- f state
    all <- takeMVar db
    putMVar db $ Map.insert sesid newState all

