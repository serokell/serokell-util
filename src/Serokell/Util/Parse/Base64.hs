{-# LANGUAGE FlexibleContexts #-}

-- | Parsing Base64

module Serokell.Util.Parse.Base64
       ( base64
       , base64Url
       ) where

import Universum hiding (fail)

import Control.Applicative (many, some, (<|>))
import Control.Monad (fail)
import Text.Parsec.Char (char)

import Serokell.Util.Base64 (decode, decodeUrl)
import Serokell.Util.Parse.Common (CharParser, asciiAlphaNum)

base64 :: CharParser ByteString
base64 = do
  str <- (++) <$> some (asciiAlphaNum <|> char '+' <|> char '/') <*> many (char '=')
  case decode $ toText str of
    Left e   -> fail $ toString e
    Right bs -> return bs

base64Url :: CharParser ByteString
base64Url = do
  str <- (++) <$> some (asciiAlphaNum <|> char '_' <|> char '-') <*> many (char '=')
  case decodeUrl $ toText str of
    Left e   -> fail $ toString e
    Right bs -> return bs
