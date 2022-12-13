module Data.Haskdaq.Helper (parserCountThenWords, parserCountThenPaddedString, parserPaddedString) where

import Data.ByteString.Internal (w2c)
import Data.Attoparsec.Combinator (count)
import Data.Attoparsec.ByteString (Parser, anyWord8)

parserCountThenWords :: Integral a => Parser a -> Parser b -> Parser [b]
parserCountThenWords cp dp = do
    n <- cp;
    d <- count (fromIntegral n) dp;
    return d

parserCountThenPaddedString :: Integral a => Int -> Parser a -> Parser String
parserCountThenPaddedString pd cp = do
    n <- cp;
    parserPaddedString pd (fromIntegral n)

parserPaddedString :: Int -> Int -> Parser String
parserPaddedString pd ln = do
    d <- count ln anyWord8;
    _ <- count (let r = ln `mod` pd in if r == 0 then 0 else pd - (fromIntegral r)) anyWord8;
    return (map w2c d)
