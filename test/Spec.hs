module Spec where

import Test.QuickCheck
import Bot
import Logger
import Control.Concurrent
import Data.Traversable(for)
import Data.List

main :: IO ()
main = do
    quickCheck testGroupMsgs
    return ()

testGroupMsgs :: [Int] -> Bool
testGroupMsgs x = let res = map fst . groupMsgs . map (\x -> (x, "")) $ x
                   in res == nub res
