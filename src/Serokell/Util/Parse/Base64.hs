{-# LANGUAGE FlexibleContexts #-}

-- | Parsing Base64

module Serokell.Util.Parse.Base64
       ( base64
       , base64Url
       ) where
import qualified Data.ByteString                    as BS
import           Data.Text                          (pack, unpack)
import           Serokell.Util.Base64               (decode, decodeUrl)
import           Serokell.Util.Parse.Common         (CharParser, asciiAlphaNum)
import           Text.Parsec                        (many, many1, (<|>))
import           Text.ParserCombinators.Parsec.Char (char)

base64 :: CharParser BS.ByteString
base64 = do
  str <- (++) <$> many1 (asciiAlphaNum <|> char '+' <|> char '/') <*> many (char '=')
  case decode $ pack str of
    Left e   -> fail $ unpack e
    Right bs -> return bs

base64Url :: CharParser BS.ByteString
base64Url = do
  str <- (++) <$> many1 (asciiAlphaNum <|> char '_' <|> char '-') <*> many (char '=')
  case decodeUrl $ pack str of
    Left e   -> fail $ unpack e
    Right bs -> return bs

