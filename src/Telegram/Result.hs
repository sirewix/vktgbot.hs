{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Result
    ( parseTgResult
    ) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

--import SerDe
import Result

parseTgResult :: (FromJSON a) => Value -> Result.Result a
parseTgResult = eitherToRes . parseEither parser
    where parser = withObject "" $ \obj -> do
             ok <- obj .:  "ok"
             if ok then
                   (obj .:  "result")
             else do
                   err <- obj .: "description"
                   fail $ "Telegram request failed with: " ++ err
          parser :: (FromJSON a) => Value -> Parser a
