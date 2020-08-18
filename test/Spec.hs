{-# LANGUAGE
    OverloadedStrings
  , StandaloneDeriving
  , TupleSections
  #-}

import           Bot                            ( BotIO
                                                , BotUserInteraction(..)
                                                , Button
                                                , Message
                                                , groupMsgs
                                                )
import           Control.Arrow                  ( second )
import           Control.Monad.Free             ( Free(..) )
import           Data.Functor.Classes           ( liftEq )
import           Data.List                      ( nub )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           EchoBot                        ( EchoBotState(..)
                                                , echoBot
                                                , mkEchoBotOptions
                                                )
import           Misc                           ( (=:) )
import           Result                         ( resToIO )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , arbitraryUnicodeChar
                                                , listOf
                                                , listOf1
                                                , quickCheck
                                                )

newtype AnyText = AnyText { unAnyText :: Text }
    deriving Show
newtype NonEmptyAnyText = NonEmptyAnyText { unNonEmptyAnyText :: Text }
    deriving Show

instance Arbitrary AnyText where
    arbitrary = AnyText . pack <$> listOf arbitraryUnicodeChar

instance Arbitrary NonEmptyAnyText where
    arbitrary = NonEmptyAnyText . pack <$> listOf1 arbitraryUnicodeChar

main :: IO ()
main = do
    putStr "Testing message grouping function\n"
    quickCheck testGroupMsgs
    opts <- resToIO $ mkEchoBotOptions [ "helpText" =: "help!" ]

    putStr "Testing echoing functionality for every number and message\n"
    quickCheck $ \(n, NonEmptyAnyText str) -> testInterpret
                            (echoBot opts)
                            (EchoBotState { nrepeat = n }) [str]
                       == (EchoBotState { nrepeat = n }, map (const (str, Nothing)) [1..n])


    putStr "Testing /help command\n"
    quickCheck $ \(n, AnyText str) -> testInterpret
                            (echoBot opts)
                            (EchoBotState { nrepeat = n }) ["/help " <> str]
                       == (EchoBotState { nrepeat = n }, [("help!", Nothing)])

    putStr "Testing /repeat commmand for every initial and subsequent numbers\n"
    quickCheck $ \(n, m, NonEmptyAnyText str) ->
        let (state, outs) = testInterpret
                                    (echoBot opts)
                                    (EchoBotState { nrepeat = n })
                                        [ "/repeat"
                                        , pack $ show m
                                        , str ]
            eq (Just msg, btns) (msg', btns') = msg == msg' && btns == btns'
            eq (Nothing, btns) (_msg', btns') = btns == btns'
         in (state == EchoBotState { nrepeat = m })
             && liftEq eq
                       ([ (Nothing, Just (map (pack . show) ([1 .. 5] :: [Int])))
                        , (Nothing, Nothing)
                        ] ++ map (const (Nothing, Nothing)) [1..m])
                       outs

testGroupMsgs :: [Int] -> Bool
testGroupMsgs x = let res = map fst . groupMsgs . map (, "" :: String) $ x
                   in res == nub res

testInterpret :: BotIO s () -> s -> [Message] -> (s, [(Message, Maybe [Button])])
testInterpret program state msgs = second reverse $ interpret' program (state, []) msgs
  where interpret' action (state, out) msgs =
            case action of
              Free (ReadMessage f) ->
                  if null msgs then (state, out)
                  else interpret' (f (head msgs)) (state, out) (tail msgs)
              Free (SendMessage m btns n) -> interpret' n (state, (m, btns) : out) msgs
              Free (ModifyState f n) -> interpret' n (f state, out) msgs
              Free (ReadState f) -> interpret' (f state) (state, out) msgs
              Pure _ -> interpret' program (state, out) msgs
