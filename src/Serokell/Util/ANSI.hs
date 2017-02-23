-- | Functions for working with ANSI-colored text.
module Serokell.Util.ANSI
       ( Color(..)
       , colorize
       , withColoredMessages
       ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Monoid            (mappend)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           System.Console.ANSI    (Color (..), ColorIntensity (Vivid),
                                         ConsoleLayer (Foreground), SGR (Reset, SetColor),
                                         setSGRCode)

-- | Prettify 'Text' message with 'Vivid' color.
colorize :: Color -> Text -> Text
colorize color msg =
    T.pack (setSGRCode [SetColor Foreground Vivid color]) `mappend`
    msg `mappend`
    T.pack (setSGRCode [Reset])

-- | Write colored message, do some action, write colored message.
-- Intended for debug only.
withColoredMessages :: MonadIO m => Color -> Text -> m a -> m a
withColoredMessages color activity action = do
    liftIO $ T.putStrLn (colorize color ("Entered " `mappend` activity))
    res <- action
    liftIO $ T.putStrLn (colorize color ("Finished " `mappend` activity))
    return res
