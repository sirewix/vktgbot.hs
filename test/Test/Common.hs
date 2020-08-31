module Test.Common where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , Testable
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

signedCheck :: Testable prop => String -> prop -> IO ()
signedCheck msg f = do
  putStrLn $ "Testing " <> msg <> " "
  quickCheck f
