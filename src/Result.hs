module Result( Result(..)
             , maybeToRes
             , eitherToRes
             , resToIO
             , resToM
             ) where

data Result a = Ok a | Err String
    deriving(Show)

instance Functor Result where
--  fmap :: (a -> b) -> Result a -> Result b
    fmap f (Ok a) = Ok(f a)
    fmap f (Err e) = Err(e)

instance Applicative Result where
    pure a = Ok a
--  (<*>) :: Result (a -> b) -> Result a -> Result b
    Ok  f <*> Ok  a = Ok (f a)
    Ok  _ <*> Err e = Err (e)
    Err e <*> _     = Err (e)

instance Monad Result where
--  (>>=) :: Result a -> (a -> Result b) -> Result b infixl 1
    Ok a >>= f = f a
    Err e >>= _ = Err e

instance MonadFail Result where
    fail e = Err e

resToIO :: Result a -> IO a
resToIO r = case r of
              Ok a -> return a
              Err a -> error a

resToM :: (Monad m, MonadFail m) => Result a -> m a
resToM r = case r of
              Ok a -> return a
              Err a -> fail a

maybeToRes :: String -> Maybe a -> Result a
maybeToRes _ (Just a) = Ok a
maybeToRes e Nothing  = Err e

eitherToRes :: Either String a -> Result a
eitherToRes (Right a) = Ok a
eitherToRes (Left err) = Err err
