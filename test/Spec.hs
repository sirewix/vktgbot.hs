{-# LANGUAGE
    OverloadedStrings
  , StandaloneDeriving
  , TupleSections
  #-}

import           Bot                            ( groupMsgs )
import           Data.List                      ( nub )
import           Test.Common                    ( signedCheck )
import           Test.EchoBot                   ( testEchoBot )
import           Test.Interpreter               ( testInterpreter )

main :: IO ()
main = do
  putChar '\n'
  signedCheck "message grouping function" testGroupMsgs
  testEchoBot
  testInterpreter

testGroupMsgs :: [Int] -> Bool
testGroupMsgs x = let res = map fst . groupMsgs . map (, "" :: String) $ x
                   in res == nub res

