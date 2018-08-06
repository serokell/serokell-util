{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Bitgram's general-purpose exceptions.
-- They may be useful when you need a simple instance of Exception (e. g. in MonadThrow context)

module Serokell.Util.Exceptions
       ( TextException (..)
       , throwText
       , EmptyException (..)
       , throwEmpty
       , eitherToFail
       ) where

import Universum

import Formatting (bprint, stext, string, (%))

import qualified Control.Monad as Monad
import qualified Formatting.Buildable

instance Formatting.Buildable.Buildable SomeException where
    build e =
        case fromException e of
            Nothing                  -> bprint string (displayException e)
            Just (TextException msg) -> bprint ("TextException: " %stext) msg

-- | Use this type if you are sure that text description is enough to represent error
newtype TextException = TextException
    { teMessage :: Text
    } deriving (Show,Typeable)

instance Exception TextException

instance Formatting.Buildable.Buildable TextException where
    build = bprint ("TextException: " %stext) . teMessage

throwText :: MonadThrow m
          => Text -> m a
throwText = throwM . TextException

-- | Use this type if you want to signal about error and there may be only one reason for it
data EmptyException = EmptyException
  deriving (Show, Typeable)

instance Exception EmptyException

throwEmpty :: MonadThrow m => m a
throwEmpty = throwM EmptyException

-- | Convert MonadThrow to arbitrary monad using `fail` to report
-- error Useful when you have `MonadThrow` and want to use it in
-- another monad context which uses `fail` for errors
eitherToFail :: (Monad m, e ~ SomeException) => Either e a -> m a
eitherToFail = either (Monad.fail . show) return
