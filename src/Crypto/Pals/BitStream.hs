
module Crypto.Pals.BitStream where


import Data.Bits ((.|.), shiftL, shiftR, xor, testBit, clearBit)
import Data.Word

import qualified Data.ByteString.Lazy as BS


getFirstNBits :: Int -> BitChunk -> (Word8, BitChunk)
getFirstNBits n (BitChunk s b) = (left, BitChunk (s - n) right)
  where
    left = b `shiftR` (s - n)
    right = (left `shiftL` (s - n)) `xor` b


-- | Get @n bits from the BitStream given as argument
getNextBits :: Int -> BitStream -> Maybe (Word8, BitStream)
getNextBits _ [] = Nothing
getNextBits n (chunk@(BitChunk s b):xs)
  | s == n = Just (b, xs)
  | s >= n =
    let (left, right) = getFirstNBits n chunk
      in Just (left, right : xs)
  | otherwise =
    case getNextBits (n - s) xs of
      Nothing -> Nothing -- There are not enough bits remaining
      Just (left, remaining) -> Just ((b `shiftL` (n - s)) .|. left, remaining)


groupByN :: Int -> BitStream -> [Word8]
groupByN _ [] = []
groupByN n xs =
  case getNextBits n xs of
    Just (chunk, remaining) ->
      chunk : groupByN n remaining
    Nothing -> -- We are missing a few bits to form a complete chunk here
      let remainingBits = getStreamSize xs
      in groupByN remainingBits xs


-- BitChunk contains maximum 8 bits
data BitChunk  = BitChunk
  { size    :: Int
  , getBits :: Word8
  } deriving (Show)

type BitStream = [BitChunk]


bytes :: BitStream -> BS.ByteString
bytes = BS.pack . groupByN 8

bits :: BS.ByteString -> BitStream
bits = map (BitChunk 8) . BS.unpack

toList :: BitStream -> [Bool]
toList = concatMap chunkToBits
  where
    chunkToBits :: BitChunk -> [Bool]
    chunkToBits (BitChunk 0 _) = []
    chunkToBits (BitChunk n b) = testBit b (n - 1) : chunkToBits (BitChunk (n - 1) (clearBit b (n - 1)))


getStreamSize :: BitStream -> Int
getStreamSize = sum . map size
