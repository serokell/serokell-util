-- | Parsing network data

module Serokell.Util.Parse.Network
       ( Host (..)
       , port
       , ipv4address
       , ipv6address
       , ipv6addressWithScope
       , hostname
       , host
       , host'
       , connection
       , connection'
       ) where

import           Control.Monad                      (liftM, void)
import           Data.Word                          (Word16)
import           Serokell.Util.Parse.Common         (Parser, asciiAlphaNum, byte,
                                                     countMinMax, limitedInt)
import           Text.Parsec                        (choice, count, many1, oneOf, option,
                                                     try, (<?>), (<|>))
import           Text.ParserCombinators.Parsec.Char (alphaNum, char, hexDigit, string)

data Host = IPv4Address { hostAddress :: String }
          | IPv6Address { hostAddress :: String }
          | HostName    { hostAddress :: String }
    deriving(Show, Eq, Ord)

concatSequence :: (Monad m) => [m [a]] -> m [a]
concatSequence = liftM concat . sequence

port :: Parser Word16
port = fromIntegral <$> limitedInt 65535 "Port number to large"

ipv4address :: Parser String
ipv4address = concatSequence [
    byteStr, string ".",
    byteStr, string ".",
    byteStr, string ".", byteStr] <?> "bad IPv4 address"
  where
    byteStr = show <$> byte

ipv6address :: Parser String
ipv6address = do
    let ipv6variants = (try <$> skippedAtBegin)
                        ++ [try full]
                        ++ (try <$> skippedAtMiddle)
                        ++ (try <$> skippedAtEnd)
                        ++ [last2 False]
    choice ipv6variants <?> "bad IPv6 address"
  where
    hexShortNum = countMinMax 1 4 hexDigit
    h4s = (++) <$> hexShortNum <*> string ":"
    sh4 = (++) <$> string ":" <*> hexShortNum
    execNum 0 = return ""
    execNum n = concat <$> count n h4s
    partNum 0 = return ""
    partNum n = do
        f <- hexShortNum
        e <- countMinMax 0 (n - 1) (try sh4)
        return $ f ++ concat e

    maybeNum n = concat <$> countMinMax 0 n h4s
    last2f = try ipv4address <|> concatSequence [h4s, hexShortNum]
    last2 f = if f
        then last2f
        else choice [try last2f,
                     try $ concatSequence [string "::", hexShortNum],
                     concatSequence [hexShortNum, string "::"]]

    skippedAtBegin =
        map (\i -> concatSequence [string "::", execNum i, last2 True]) [5,4..0]

    skippedAtMiddle = [
        concatSequence [partNum 1, string "::", maybeNum 4, last2 True],
        concatSequence [partNum 2, string "::", maybeNum 3, last2 True],
        concatSequence [partNum 3, string "::", maybeNum 2, last2 True],
        concatSequence [partNum 4, string "::", maybeNum 1, last2 True],
        concatSequence [partNum 5, string "::", last2 True],
        concatSequence [partNum 6, string "::", hexShortNum]]

    skippedAtEnd = [concatSequence [partNum 7, string "::"]]

    full = concatSequence [concat <$> count 6 h4s, last2 True]

ipv6addressWithScope :: Parser String
ipv6addressWithScope = concatSequence [ipv6address, option "" scope]
  where
    scope = concatSequence [string "%", many1 asciiAlphaNum]

hostname :: Parser String
hostname = many1 $ alphaNum <|> oneOf ".-_"

host :: Parser String
host = hostAddress <$> host'

host' :: Parser Host
host' = (IPv6Address <$> try ipv6str)
         <|> (IPv4Address <$> try ipv4address)
         <|> (HostName <$> hostname)
  where
    ipv6str = do
        void $ char '['
        ipv6 <- ipv6addressWithScope
        void $ char ']'
        return ipv6

connection :: Parser (String, Maybe Word16)
connection = (\(h, p) -> (hostAddress h, p)) <$> connection'

connection' :: Parser (Host, Maybe Word16)
connection' = do
    addr <- host'
    p <- maybePort
    return (addr, p)
  where
    maybePort = option Nothing $ char ':' >> Just <$> port
