{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions for working with ANSI-colored text.
module Serokell.Util.ANSI
       ( Color(..)
       , colorize
       , colorizeDull
       , withColoredMessages
       ) where

import Universum

import System.Console.ANSI (Color (..), ConsoleLayer (Foreground), SGR (Reset, SetColor),
                            setSGRCode)

import qualified System.Console.ANSI as ANSI

-- | Prettify 'Text' message with 'Vivid' color.
colorize :: Color -> Text -> Text
colorize = colorizeImpl ANSI.Vivid

-- | Colorize text using 'ANSI.Dull' palete (in contrast to 'colorize'
-- which uses 'ANSI.Vivid' palete)
colorizeDull :: Color -> Text -> Text
colorizeDull = colorizeImpl ANSI.Dull

colorizeImpl :: ANSI.ColorIntensity -> Color -> Text -> Text
colorizeImpl palete color msg =
    toText (setSGRCode [SetColor Foreground palete color]) <>
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
