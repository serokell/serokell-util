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

import           Control.Exception      (Exception, SomeException,
                                         fromException)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Data.Text              (Text)
import           Data.Text.Buildable    (Buildable (build))
import qualified Data.Text.Format       as F
import           Data.Text.Lazy.Builder (Builder)
import           Data.Typeable          (Typeable)

instance Buildable SomeException where
    build e =
        maybe (build $ F.Shown e) (build :: TextException -> Builder) $
        fromException e

-- | Use this type if you are sure that text description is enough to represent error
newtype TextException = TextException
    { teMessage :: Text
    } deriving (Show,Typeable)

instance Exception TextException

instance Buildable TextException where
    build = F.build "TextException: {}" . F.Only . teMessage

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
eitherToFail = either (fail . show) return
