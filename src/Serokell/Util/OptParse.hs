-- | Some useful helper for optparse-applicative library

module Serokell.Util.OptParse
       ( fromStr
       , strArgument
       , strOption
       ) where

import           Data.String         (IsString (fromString))
import           Options.Applicative (ArgumentFields, Mod, OptionFields, Parser,
                                      ReadM, argument, option, str)

-- | Reader which uses IsString instance for parsing
fromStr :: IsString s => ReadM s
fromStr = fromString <$> str

-- | Parse argument using IsString instance
strArgument :: IsString s => Mod ArgumentFields s -> Parser s
strArgument = argument fromStr

-- | Parse option using IsString instance
strOption :: IsString s => Mod OptionFields s -> Parser s
strOption = option fromStr
