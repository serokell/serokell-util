{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions for working with ANSI-colored text.
module Serokell.Util.ANSI
       ( Color(..)
       , colorize
       , withColoredMessages
       ) where

import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                            SGR (Reset, SetColor), setSGRCode)
import Universum

-- | Prettify 'Text' message with 'Vivid' color.
colorize :: Color -> Text -> Text
colorize color msg =
    toText (setSGRCode [SetColor Foreground Vivid color]) <>
    msg <>
    toText (setSGRCode [Reset])

-- | Write colored message, do some action, write colored message.
-- Intended for debug only.
withColoredMessages :: MonadIO m => Color -> Text -> m a -> m a
withColoredMessages color activity action = do
    putStrLn (colorize color ("Entered " <> activity))
    res <- action
    putStrLn (colorize color ("Finished " <> activity))
    return res
