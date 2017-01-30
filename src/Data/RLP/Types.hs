{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# OPTIONS -fno-warn-orphans  #-}
module Data.RLP.Types where

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8

import           Data.Bits
import           Data.Char             (ord)
import           Data.Foldable
import           Data.Int
import           Data.List             (foldl')
import           Data.Word

data RLPObject = String S.ByteString | Array [RLPObject] deriving (Eq, Ord, Read, Show)

class RLPEncodable a where
    rlpEncode :: a -> RLPObject
    rlpDecode :: RLPObject -> Either String a

rlpEncodeFinite :: (FiniteBits n, Integral n) => n -> RLPObject
rlpEncodeFinite = rlpEncode . S.pack . packFiniteBE
{-# INLINE rlpEncodeFinite #-}

rlpDecodeIntegralBE :: (Bits n, Integral n) => RLPObject -> Either String n
rlpDecodeIntegralBE = \case
    String s -> Right . unpackBE $ S.unpack s
    x        -> rlpDecodeFail "String" x
{-# INLINE rlpDecodeIntegralBE #-}

rlpDecodeFail :: String -> RLPObject -> Either String a
rlpDecodeFail myType instead =
    Left $ "Expected an RLPObject that's isomorphic to " ++ myType ++ ", instead got " ++ show instead
{-# INLINE rlpDecodeFail #-}

-- todo: more efficient?
unpackBE :: (Bits n, Integral n) => [Word8] -> n
unpackBE words = foldl' (.|.) 0 shifted
    where shifts  = [((wc - 1) * 8), ((wc - 2) * 8)..0]
          wc      = length words
          doShift word shift = fromIntegral word `shiftL` shift
          shifted = zipWith doShift words shifts
{-# INLINE unpackBE #-}

-- todo: ditto
packFiniteBE :: (FiniteBits n, Integral n) => n -> [Word8]
packFiniteBE n = packWithByteCount byteCount n
    where byteCount = (finiteBitSize n + 7) `quot` 8
{-# INLINE packFiniteBE #-}

packIntegerBE :: Integer -> [Word8]
packIntegerBE n = packWithByteCount byteCount n
    where byteCount = (bitCount + 7) `quot` 8
          bitCount  = floor (logBase 2 $ fromIntegral n) + 1
{-# INLINE packIntegerBE #-}

packWithByteCount :: (Bits n, Integral n) => Int -> n -> [Word8]
packWithByteCount byteCount n = dropWhile (== 0) $ zipWith f rep shifts
    where rep    = replicate byteCount n
          shifts = [((byteCount - 1) * 8), ((byteCount - 2) * 8)..0]
          f r s  = fromIntegral (r `shiftR` s)
{-# INLINE packWithByteCount #-}

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
    rlpEncode = rlpEncodeFinite
    rlpDecode = rlpDecodeIntegralBE

instance RLPEncodable Word16 where
    rlpEncode = rlpEncodeFinite
    rlpDecode = rlpDecodeIntegralBE

instance RLPEncodable Word32 where
    rlpEncode = rlpEncodeFinite
    rlpDecode = rlpDecodeIntegralBE

instance RLPEncodable Word64 where
    rlpEncode = rlpEncodeFinite
    rlpDecode = rlpDecodeIntegralBE

instance (RLPEncodable a) => RLPEncodable [a] where
    rlpEncode = Array . toList . fmap rlpEncode
    rlpDecode = \case
        Array xs -> sequence $ rlpDecode <$> xs
        x        -> rlpDecodeFail "Array" x

instance RLPEncodable RLPObject where -- ayy lmao
    rlpEncode = id
    rlpDecode = Right

instance RLPEncodable Integer where
    rlpEncode = rlpEncode . S.pack . packIntegerBE
    rlpDecode = rlpDecodeIntegralBE

instance RLPEncodable Char where
    rlpEncode = rlpEncodeFinite . ord
    rlpDecode = \case
        String s | S.length s == 1 -> Right (S8.head s)
        x                          -> rlpDecodeFail "String of length 1" x
