{-# LANGUAGE MultiParamTypeClasses #-}

-- | Some useful helper for optparse-applicative library

module Serokell.Util.OptParse
       ( fromStr
       , fromParsec
       ) where

import Universum

import Data.String (IsString (fromString))
import Options.Applicative (ReadM, eitherReader, str)
import Text.Parsec (Parsec, parse)

-- | Reader which uses IsString instance for parsing
fromStr :: IsString s => ReadM s
fromStr = fromString <$> str

fromParsec :: Parsec Text () a -> ReadM a
fromParsec parser =
    eitherReader $ first show . parse parser "<CLI options>" . toText
