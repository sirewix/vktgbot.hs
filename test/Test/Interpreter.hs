{-# LANGUAGE OverloadedStrings #-}
module Test.Interpreter where

import           Bot                            ( interpret )
import           Bot.API                        ( BotAPI(..) )
import           Bot.IO                         ( BotState(..)
                                                , readMessage
                                                , sendMessage
                                                , modifyState
                                                , readState
                                                )
import           Control.Concurrent.MVar        ( newMVar
                                                , modifyMVar_
                                                , readMVar
                                                )
import           Control.Monad.Except           ( liftIO )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.Free             ( Free(..) )
import           Data.Text                      ( Text )
import           Logger                         ( Logger )
import           Test.Common                    ( NonEmptyAnyText(..)
                                                , signedCheck
                                                )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , Property
                                                )
import           Test.QuickCheck.Function       ( Fun(..) )
import           Test.QuickCheck.Monadic        ( PropertyM
                                                , assert
                                                , monadicIO
                                                , pick
                                                , run
                                                )

mocklog :: Logger
mocklog _ _ = return ()

testInterpreter :: IO ()
testInterpreter = do
  signedCheck "ReadMessage" testReadMessage
  signedCheck "SendMessage" testSendMessage
  signedCheck "ReadState"   testReadState
  signedCheck "ModifyState" testModifyState

testReadMessage :: Property
testReadMessage = monadicIO $ do
  msg <- pick arbitrary
  initState <- pick arbitrary
  let state = BotState (initState :: Int) readMessage
  let condition (res, state') = case msg of
        Just (NonEmptyAnyText msg) -> Just msg == res
        Nothing -> content state == content state'
  runInterpreter "" (BotAPI undefined undefined) state msg condition

testSendMessage :: Property
testSendMessage = monadicIO $ do
  bin                 <- run $ newMVar ("" :: Text)
  NonEmptyAnyText msg <- pick arbitrary
  initState           <- pick arbitrary
  let state = BotState (initState :: Int) (sendMessage msg)
      apiSendMessage _ msg _ = liftIO $ modifyMVar_ bin (const $ pure msg)
      condition (_res, state') = content state == content state'
  runInterpreter ()
                 (BotAPI apiSendMessage undefined)
                 state
                 (Just $ NonEmptyAnyText msg)
                 condition
  res <- run $ readMVar bin
  assert $ res == msg

testReadState :: Property
testReadState = monadicIO $ do
  initState <- pick arbitrary
  msg       <- pick arbitrary
  let state = BotState (initState :: Int) readState
      condition (res, state') = Just initState == res && content state == content state'
  runInterpreter 0 (BotAPI undefined undefined) state msg condition

testModifyState :: Property
testModifyState = monadicIO $ do
  msg       <- pick arbitrary
  initState <- pick arbitrary
  Fun _ f   <- pick arbitrary
  let state = BotState (initState :: Int) (modifyState f)
      condition (_res, state') = f (content state) == content state'
  runInterpreter () (BotAPI undefined undefined) state msg condition

runInterpreter
  :: a
  -> BotAPI Int
  -> BotState s a
  -> Maybe NonEmptyAnyText
  -> ((Maybe a, BotState s a) -> Bool)
  -> PropertyM IO ()
runInterpreter init api state msg condition = do
  res <- run . runExceptT $ interpret api
                                      mocklog
                                      (0 :: Int)
                                      (Pure init)
                                      state
                                      (unNonEmptyAnyText <$> msg)
  either (const $ assert False) (assert . condition) res
