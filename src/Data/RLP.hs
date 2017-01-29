{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
module Data.RLP
    ( RLPObject(..)
    , RLPEncodable(..)
    , rlpParser
    , unpackRLP
    , unpackRLPFully
    , packRLP
    , module Data.RLP.Types
    ) where

import           Control.Applicative        ((<|>))
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.Combinator
import           Data.Bits                  (Bits, FiniteBits, finiteBitSize,
                                             shiftL, shiftR, (.|.))
import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as S8
import           Data.List                  (foldl', intercalate)
import           Data.Word
import           Numeric                    (showHex)
import           Prelude                    hiding (take)
import qualified Prelude                    as P

import           Data.RLP.Types

singleByteParser :: Parser RLPObject
singleByteParser = (String . S.singleton) <$> satisfy (<= 0x7F)

shortStringParser :: Parser RLPObject
shortStringParser = do
    length <- fromIntegral . subtract 0x80 <$> satisfy (\x -> x >= 0x80 && x <= 0xB7)
    String <$> take length

longStringParser :: Parser RLPObject
longStringParser = do
    lengthLength <- fromIntegral . subtract 0xB7 <$> satisfy (\x -> x >= 0xB8 && x <= 0xBF)
    length <- unpackNumBE . S.unpack <$> take lengthLength
    String <$> take length

parseListPayload :: S.ByteString -> Parser [RLPObject]
parseListPayload pl = case parse rlpParser pl of
    Done rem res -> if S8.null rem
        then return [res]
        else (res:) <$> parseListPayload rem
    Partial _    ->
        fail "Partial result when parsing an RLP list member, this should be impossible."
    Fail rem ctxs err ->
        fail $ "RLP list member parse failed: " ++ intercalate ", " ctxs ++ ": " ++ err
               ++ ". Remaining data: \"" ++ S8.unpack rem ++ "\""

shortListParser :: Parser RLPObject
shortListParser = do
    payloadLength <- fromIntegral . subtract 0xC0 <$> satisfy (\x -> x >= 0xC0 && x <= 0xF7)
    if payloadLength == 0
        then return (Array [])
        else Array <$> (take payloadLength >>= parseListPayload)

longListParser :: Parser RLPObject
longListParser = do
    lengthLength <- fromIntegral . subtract 0xF7 <$> satisfy (\x -> x >= 0xF8 && x <= 0xFF)
    payloadLength <- unpackNumBE . S.unpack <$> take lengthLength
    if payloadLength == 0
        then return (Array [])
        else Array <$> (take payloadLength >>= parseListPayload)

rlpParser :: Parser RLPObject
rlpParser =  try (singleByteParser  <?> "single byte")
         <|> try (longStringParser  <?> "long string")  -- long string/list go first since we dont want the error
         <|> try (shortStringParser <?> "short string") -- message saying it failed parsing a long list cause it fell
         <|> try (longListParser    <?> "long list")    -- through after failing to correctly parse a short one
         <|> try (shortListParser   <?> "short list")

unpackRLP :: S.ByteString -> Either String RLPObject
unpackRLP input = case parseOnly rlpParser input of
    Left err -> Left $ "Parse failed: " ++ err -- to have consistent errors w/ `parseRLPFully`
    r        -> r

unpackRLPFully :: S.ByteString -> Either String RLPObject
unpackRLPFully input = handleResult $ parse rlpParser input
    where handleResult = \case
            Done rem res -> if S8.null rem
                then Right res
                else Left $ "Incomplete parse, leftover data: " ++ S8.unpack rem
            Fail rem ctxs err ->
                Left $ "Parse failed: " ++ intercalate ", " ctxs ++ ": " ++ err
                       ++ ". Remaining data: \"" ++ S8.unpack rem ++ "\""
            Partial cont -> handleResult (cont S8.empty)

packRLP :: RLPObject -> S.ByteString
packRLP o = case o of
    String s -> packString s
    Array xs -> packList xs
    where packString s | len == 0  = S.singleton 0x80
                       | len == 1  = packSingleChar (S.head s)
                       | len <= 55 = S.cons (0x80 + fromIntegral len) s
                       | otherwise = prefixLength 0xB7 s
                       where len = S.length s

          packSingleChar c | c <= 0x7F = S.singleton c
                           | otherwise = S.pack [0x81, c]

          packList xs | payloadLength <= 55 = S.cons (0xC0 + fromIntegral payloadLength) packedPayload
                      | otherwise = prefixLength 0xF7 packedPayload
                      where packedPayload = S.concat (packRLP <$> xs)
                            payloadLength = S.length packedPayload

          prefixLength base str = (prefixed `S.cons` pLen) `S.append` str
              where len      = S.length str
                    pLen     = S.pack (packNumBE len)
                    pLenLen  = fromIntegral (S.length pLen)
                    prefixed = base + pLenLen
