module Storage
  ( newStorage
  , updateStorage
  , Storage
  )
where


import           Control.Concurrent
import           Data.Hashable
import           Data.Maybe
import qualified Data.HashMap.Strict           as Map

type Storage sesid state = MVar (Map.HashMap sesid state)

newStorage :: (Eq sesid, Hashable sesid) => IO (Storage sesid state)
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
  let state = fromMaybe defState $ Map.lookup sesid all
  newState <- f state
  all      <- takeMVar db
  putMVar db $ Map.insert sesid newState all

