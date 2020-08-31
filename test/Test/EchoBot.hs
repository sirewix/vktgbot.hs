{-# LANGUAGE OverloadedStrings #-}
module Test.EchoBot where

import           EchoBot.Options                ( mkEchoBotOptions )
import           EchoBot                        ( EchoBotState(..)
                                                , echoBot
                                                )
import           Bot.IO                         ( BotIO
                                                , BotUserInteraction(..)
                                                , Button
                                                , Message
                                                )
import           Control.Arrow                  ( second )
import           Control.Monad.Free             ( Free(..) )
import           Data.Functor.Classes           ( liftEq )
import           Data.Text                      ( pack )
import           Misc                           ( (=:) )
import           Test.Common                    ( AnyText(..)
                                                , NonEmptyAnyText(..)
                                                , signedCheck
                                                )

testEchoBot :: IO ()
testEchoBot = do
    let program = echoBot $ mkEchoBotOptions [ "helpText" =: "help!" ]
    putStrLn "Testing echo bot logic"
    signedCheck "echoing functionality for every number and message" (testEchoing program)
    signedCheck "/help command" (testHelp program)
    signedCheck "/repeat commmand for every initial and subsequent numbers" (testRepeat program)

testHelp :: BotIO EchoBotState () -> (Int, AnyText) -> Bool
testHelp program (n, AnyText str) =
  pureInterpreter program (EchoBotState { nrepeat = n }) ["/help " <> str]
    == (EchoBotState { nrepeat = n }, [("help!", Nothing)])

testRepeat :: BotIO EchoBotState () -> (Int, Int, NonEmptyAnyText) -> Bool
testRepeat program (n, m, NonEmptyAnyText str) =
  let (state, outs) = pureInterpreter program
                                      (EchoBotState { nrepeat = n })
                                      ["/repeat", pack $ show m, str]
      eq (Just msg, btns) (msg' , btns') = msg == msg' && btns == btns'
      eq (Nothing , btns) (_msg', btns') = btns == btns'
  in  (state == EchoBotState { nrepeat = m })
        && liftEq
             eq
             (  [ (Nothing, Just (map (pack . show) ([1 .. 5] :: [Int])))
                , (Nothing, Nothing)
                ]
             ++ map (const (Nothing, Nothing)) [1 .. m]
             )
             outs

testEchoing :: BotIO EchoBotState () -> (Int, NonEmptyAnyText) -> Bool
testEchoing program (n, NonEmptyAnyText str) =
  pureInterpreter program (EchoBotState { nrepeat = n }) [str]
    == (EchoBotState { nrepeat = n }, map (const (str, Nothing)) [1 .. n])

pureInterpreter :: BotIO s () -> s -> [Message] -> (s, [(Message, Maybe [Button])])
pureInterpreter program state msgs = second reverse $ interpret' program (state, []) msgs
  where interpret' action (state, out) msgs =
            case action of
              Free (ReadMessage f) ->
                  if null msgs then (state, out)
                  else interpret' (f (head msgs)) (state, out) (tail msgs)
              Free (SendMessage m btns n) -> interpret' n (state, (m, btns) : out) msgs
              Free (ModifyState f n) -> interpret' n (f state, out) msgs
              Free (ReadState f) -> interpret' (f state) (state, out) msgs
              Pure _ -> interpret' program (state, out) msgs
