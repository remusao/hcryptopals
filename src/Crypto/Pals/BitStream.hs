
module Crypto.Pals.BitStream where


import Data.Bits ((.|.), shiftL, shiftR, xor, testBit, clearBit)
import Data.Word

import qualified Data.ByteString.Lazy as BS


-- BitChunk contains maximum 8 bits
data BitChunk  = BitChunk
  { size    :: {-# UNPACK #-} !Int
  , getBits :: {-# UNPACK #-} !Word8
  } deriving (Show, Eq)

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

-- N should be lower or equal to 8
groupByN :: Int -> BitStream -> [Word8]
groupByN _ [] = []
groupByN n xs
  | n > 8     = error "n should be <= 8"
  | otherwise =
    case getNextBits n xs of
      Just (chunk, remaining) ->
        chunk : groupByN n remaining
      Nothing ->
        -- We are missing a few bits to form a complete chunk here
        let remainingBits = getStreamSize xs
        in if remainingBits == 0
          then []
          else
            case getNextBits remainingBits xs of
              Nothing -> error "Should not happen??"
              Just (b, _) -> [b `shiftL` (n - remainingBits)]
