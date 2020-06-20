module Misc( readT
           ) where

import Data.Maybe(listToMaybe)

readT :: (Read a) => String -> Maybe a
readT str = fst <$> listToMaybe (reads str)
