{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- | Parsing common helpers

module Serokell.Util.Parse.Common
       ( CharParser
       , countMinMax
       , limitedInt
       , byte
       , asciiAlphaNum
       ) where

import           Text.Parsec                        (Parsec, ParsecT, Stream, option,
                                                     satisfy)
import           Text.ParserCombinators.Parsec.Char (digit)

type CharParser a = forall s u m. Stream s m Char => ParsecT s u m a

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isAsciiNum :: Char -> Bool
isAsciiNum c = (c >= '0' && c <= '9')

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiAlpha c || isAsciiNum c

asciiAlphaNum :: CharParser Char
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

limitedInt :: Int -> String -> CharParser Int
limitedInt x e = do
    b <- read <$> countMinMax 1 (intDigits x) digit
    if b > x
        then fail e
        else return b
  where
    intDigits = length . show

byte :: CharParser Word
byte = fromIntegral <$> limitedInt 255 "Value to large"

