{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# OPTIONS -fno-warn-orphans  #-}
module Data.RLP.Types where

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8

import           Data.Bits
import           Data.Char             (ord)
import           Data.Int
import           Data.List             (foldl')
import           Data.Word

data RLPObject = String S.ByteString | Array [RLPObject] deriving (Eq, Ord, Read, Show)

class RLPEncodable a where
    rlpEncode :: a -> RLPObject
    rlpDecode :: RLPObject -> Either String a

rlpEncodeIntegral :: (FiniteBits n, Integral n) => n -> RLPObject
rlpEncodeIntegral = rlpEncode . S.pack . packNumBE

rlpDecodeIntegral :: (FiniteBits n, Integral n) => RLPObject -> Either String n
rlpDecodeIntegral = \case
    String s -> Right . unpackNumBE $ S.unpack s
    x        -> rlpDecodeFail "String" x

rlpDecodeFail :: String -> RLPObject -> Either String a
rlpDecodeFail myType instead =
    Left $ "Expected an RLPObject that's isomorphic to " ++ myType ++ ", instead got " ++ show instead

-- todo: more efficient?
unpackNumBE :: (FiniteBits n, Integral n) => [Word8] -> n
unpackNumBE words = foldl' (.|.) 0 shifted
    where shifts  = reverse (take (length words) [0, 8..])
          doShift word shift = fromIntegral word `shiftL` shift
          shifted = zipWith doShift words shifts

-- todo: ditto
packNumBE :: (FiniteBits n, Integral n) => n -> [Word8]
packNumBE n = dropZeros . reverse $ zipWith f rep [0, 8..]
    where byteCount = finiteBitSize n `div` 8
          rep       = replicate byteCount n
          f rep sft = fromIntegral (rep `shiftR` sft)
          dropZeros = dropWhile (== 0)

instance RLPEncodable S.ByteString where
    rlpEncode = String
    rlpDecode = \case
        String s -> Right s
        x        -> rlpDecodeFail "String" x

instance RLPEncodable String where
    rlpEncode = String . S8.pack
    rlpDecode = \case
        String s -> Right (S8.unpack s)
        x        -> rlpDecodeFail "String" x

instance RLPEncodable Int where
    rlpEncode = rlpEncodeIntegral
    rlpDecode = rlpDecodeIntegral

instance RLPEncodable Int8 where
    rlpEncode = rlpEncodeIntegral
    rlpDecode = rlpDecodeIntegral

instance RLPEncodable Int16 where
    rlpEncode = rlpEncodeIntegral
    rlpDecode = rlpDecodeIntegral

instance RLPEncodable Int32 where
    rlpEncode = rlpEncodeIntegral
    rlpDecode = rlpDecodeIntegral

instance RLPEncodable Int64 where
    rlpEncode = rlpEncodeIntegral
    rlpDecode = rlpDecodeIntegral

instance RLPEncodable Word8 where
    rlpEncode = rlpEncodeIntegral
    rlpDecode = rlpDecodeIntegral

instance RLPEncodable Word16 where
    rlpEncode = rlpEncodeIntegral
    rlpDecode = rlpDecodeIntegral

instance RLPEncodable Word32 where
    rlpEncode = rlpEncodeIntegral
    rlpDecode = rlpDecodeIntegral

instance RLPEncodable Word64 where
    rlpEncode = rlpEncodeIntegral
    rlpDecode = rlpDecodeIntegral

instance RLPEncodable Char where
    rlpEncode = rlpEncodeIntegral . ord
    rlpDecode = \case
        String s | S.length s == 1 -> Right (S8.head s)
        x                          -> rlpDecodeFail "String of length 1" x
