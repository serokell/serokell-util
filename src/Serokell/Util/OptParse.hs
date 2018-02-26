{-# LANGUAGE MultiParamTypeClasses #-}

-- | Some useful helper for optparse-applicative library

module Serokell.Util.OptParse
       ( fromParsec
       ) where

import Universum

import Options.Applicative (ReadM, eitherReader)
import Text.Parsec (Parsec, parse)

fromParsec :: Parsec Text () a -> ReadM a
fromParsec parser =
    eitherReader $ first show . parse parser "<CLI options>" . toText
