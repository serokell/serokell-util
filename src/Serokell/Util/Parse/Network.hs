{-# LANGUAGE FlexibleContexts #-}

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
       , recipient
       ) where

import Control.Applicative (some, (<|>))
import Control.Monad (liftM, void)
import Data.Monoid ((<>))
import Data.Word (Word16)
import Serokell.Util.Parse.Common (CharParser, asciiAlphaNum, byte, countMinMax, limitedInt)
import Text.Parsec (choice, count, oneOf, option, try, (<?>))
import Text.Parsec.Char (alphaNum, char, hexDigit, string)

data Host = IPv4Address { hostAddress :: String }
          | IPv6Address { hostAddress :: String }
          | HostName    { hostAddress :: String }
    deriving(Show, Eq, Ord)

concatSequence :: (Monad m) => [m [a]] -> m [a]
concatSequence = liftM concat . sequence

port :: CharParser Word16
port = fromIntegral <$> limitedInt 65535 "Port number to large"

ipv4address :: CharParser String
ipv4address = concatSequence [
    byteStr, string ".",
    byteStr, string ".",
    byteStr, string ".", byteStr] <?> "bad IPv4 address"
  where
    byteStr = show <$> byte

ipv6address :: CharParser String
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

ipv6addressWithScope :: CharParser String
ipv6addressWithScope = concatSequence [ipv6address, option "" scope]
  where
    scope = concatSequence [string "%", some asciiAlphaNum]

hostname :: CharParser String
hostname = some $ alphaNum <|> oneOf ".-_"

host :: CharParser String
host = hostAddress <$> host'

host' :: CharParser Host
host' = (IPv6Address <$> try ipv6str)
         <|> (IPv4Address <$> try ipv4address)
         <|> (HostName <$> hostname)
  where
    ipv6str = do
        void $ char '['
        ipv6 <- ipv6addressWithScope
        void $ char ']'
        return ipv6

connection' :: CharParser (Host, Maybe Word16)
connection' = do
    addr <- host'
    p <- maybePort
    return (addr, p)
  where
    maybePort = option Nothing $ char ':' >> Just <$> port

connection :: CharParser (String, Maybe Word16)
connection = (\(h, p) -> (hostAddress h, p)) <$> connection'

-- | 'Parser' for host with both hostname and port.
-- Example: 54.122.0.255:9999
recipient :: CharParser (String, Word16)
recipient = connection >>= \(h, mp) -> case mp of
              Just p -> pure (h, p)
              _      -> fail $ "No port specified for host " <> h
