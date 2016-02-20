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

import           Control.Exception   (Exception, SomeException)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Text           (Text)
import           Data.Typeable       (Typeable)

-- | Use this type if you are sure that text description is enough to represent error
newtype TextException = TextException { teMessage :: Text }
  deriving (Show, Typeable)

instance Exception TextException

throwText :: MonadThrow m
          => Text -> m a
throwText = throwM . TextException

-- | Use this type if you want to signal about error and there may be only one reason for it
data EmptyException = EmptyException
  deriving (Show, Typeable)

instance Exception EmptyException

throwEmpty :: MonadThrow m
           => m a
throwEmpty = throwM EmptyException

-- | Convert MonadThrow to arbitrary monad using `fail` to report error
-- Useful when you have `MonadThrow` and want to use it in another monad context which uses `fail` for errors
eitherToFail :: (Monad m, e ~ SomeException)
             => Either e a -> m a
eitherToFail = either (fail . show) return
