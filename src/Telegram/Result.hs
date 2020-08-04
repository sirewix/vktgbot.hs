{-# LANGUAGE OverloadedStrings #-}

module Telegram.Result
  ( parseTgResult
  )
where

import           Data.Aeson
import           Data.Aeson.Types
import           Result

parseTgResult :: (FromJSON a) => Value -> Result.Result a
parseTgResult v = eitherToRes =<< (eitherToRes . parseEither parser $ v)
 where
  parser = withObject "" $ \obj -> do
    ok <- obj .: "ok"
    if ok
      then do
        res <- obj .: "result"
        return . Right $ res
      else do
        err <- obj .: "description"
        return . Left $ "request failed with " <> err
