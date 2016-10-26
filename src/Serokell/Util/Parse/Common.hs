{-# LANGUAGE MultiParamTypeClasses #-}

-- | Parsing common helpers

module Serokell.Util.Parse.Common
       ( Parser
       , countMinMax
       , limitedInt
       , byte
       , asciiAlphaNum
       ) where

import           Text.Parsec                        (Parsec, ParsecT, Stream, option,
                                                     satisfy)
import           Text.ParserCombinators.Parsec.Char (digit)

type Parser = Parsec String ()

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isAsciiNum :: Char -> Bool
isAsciiNum c = (c >= '0' && c <= '9')

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiAlpha c || isAsciiNum c

asciiAlphaNum :: Parser Char
asciiAlphaNum = satisfy isAsciiAlphaNum

countMinMax :: (Stream s m t) => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
countMinMax m x p
    | m > 0 = do
        f <- p
        end <- countMinMax (m - 1) (x - 1) p
        return $ f : end
    | x <= 0 = return []
    | otherwise = option [] $ do
        f <- p
        end <- countMinMax 0 (x - 1) p
        return $ f : end

limitedInt :: Int -> String -> Parser Int
limitedInt x e = do
    b <- read <$> countMinMax 1 (intDigits x) digit
    if b > x
        then fail e
        else return b
  where
    intDigits = length . show

byte :: Parser Word
byte = fromIntegral <$> limitedInt 255 "Value to large"

