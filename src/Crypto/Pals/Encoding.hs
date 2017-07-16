{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.Pals.Encoding where

import Control.DeepSeq

import Data.List (elemIndex)
import Data.Char (chr, ord)
import Data.Word

import qualified Data.ByteString.Lazy as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import Crypto.Pals.BitStream


generateBytesToBitsTable :: String -> Int -> V.Vector BitChunk
generateBytesToBitsTable symbols padding = V.generate 127 gen
  where
    gen i =
      case chr i `elemIndex` symbols of
        Just n ->  BitChunk padding (fromIntegral n)
        Nothing -> error "This character is not valid for the given encoding"


generateBitsToBytesTable :: String -> VU.Vector Word8
generateBitsToBytesTable = VU.fromList . map (fromIntegral . ord)


toBits :: B.ByteString -> V.Vector BitChunk -> BitStream
toBits str fromBase = map ((V.!) fromBase . fromIntegral) . B.unpack $ str


fromBits :: BitStream -> Int -> VU.Vector Word8 -> B.ByteString
fromBits stream n toBase = B.pack . map ((VU.!) toBase . fromIntegral) $ chunks
  where
    chunks = groupByN n stream


-- | Hex to/from bits
--

newtype Hex = Hex B.ByteString
  deriving (Show, Eq, NFData)

hexSymbols :: String
hexSymbols = ['0'..'9'] ++ ['a'..'f']

hexToBits :: B.ByteString -> BitStream
hexToBits bs = toBits bs fromHex
  where
    fromHex = generateBytesToBitsTable hexSymbols 4

bitsToHex :: BitStream -> B.ByteString
bitsToHex stream = fromBits stream 4 toHex
  where
    toHex = generateBitsToBytesTable hexSymbols

hex :: B.ByteString -> Hex
hex = Hex . bitsToHex . bits

unhex :: Hex -> B.ByteString
unhex (Hex bs) = bytes . hexToBits $ bs

-- | Base64 to/from bits
--

newtype B64 = B64 B.ByteString
  deriving (Show, Eq, NFData)


b64Symbols :: String
b64Symbols = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']


b64ToBits :: B.ByteString -> BitStream
b64ToBits bs = toBits bs fromB64
  where
    fromB64 = generateBytesToBitsTable b64Symbols 6

bitsToB64 :: BitStream -> B.ByteString
bitsToB64 stream = fromBits stream 6 toB64
  where
    toB64 = generateBitsToBytesTable b64Symbols


b64 :: B.ByteString -> B64
b64 = B64 . bitsToB64 . bits

unb64 :: B64 -> B.ByteString
unb64 (B64 bs) = bytes . b64ToBits $ bs


-- | Direct convertion from hex to base64
--

hex2base64 :: B.ByteString -> B.ByteString
hex2base64 str = bitsToB64 . hexToBits $! str
