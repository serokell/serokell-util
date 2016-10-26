-- | Parsing Base64

module Serokell.Util.Parse.Base64
       ( base64
       , base64Url
       ) where
import           Data.String                        (IsString (fromString))
import           Data.Text                          (pack, unpack)
import           Serokell.Util.Base64               (decode, decodeUrl)
import           Serokell.Util.Parse.Common         (Parser, asciiAlphaNum, byte,
                                                     countMinMax, limitedInt)
import           Text.Parsec                        (many, many1, satisfy, try, (<?>),
                                                     (<|>))
import           Text.ParserCombinators.Parsec.Char (char, string)

base64 :: IsString s => Parser s
base64 = do
  str <- (++) <$> many1 (asciiAlphaNum <|> char '+' <|> char '/') <*> many (char '=')
  case decode $ pack str of
    Left e  -> fail $ unpack e
    Right _ -> return $ fromString str

base64Url :: IsString s => Parser s
base64Url = do
  str <- (++) <$> many1 (asciiAlphaNum <|> char '_' <|> char '-') <*> many (char '=')
  case decodeUrl $ pack str of
    Left e  -> fail $ unpack e
    Right _ -> return $ fromString str

