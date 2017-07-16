
module Crypto.Pals.Xor where

import Data.Foldable
import Data.Word
import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as B

import Crypto.Pals.Encoding (Hex, hex, unhex)


xor :: Hex -> Hex -> Maybe Hex
xor h1 h2 = hex <$> xorBS (unhex h1) (unhex h2)

xorBS :: B.ByteString -> B.ByteString -> Maybe B.ByteString
xorBS bs1 bs2
  | B.length bs1 /= B.length bs2 = Nothing
  | otherwise = Just . unsafeXorBS bs1 $ bs2

unsafeXorBS :: B.ByteString -> B.ByteString -> B.ByteString
unsafeXorBS bs1 = B.pack . B.zipWith Bits.xor bs1

crackXor :: Hex -> B.ByteString
crackXor h = snd . maximumBy (\(s1, _) (s2, _) -> compare s1 s2). map ((\xored -> (score xored, xored)) . unsafeXorBS bs . B.replicate size) $ candidates
  where
    bs = unhex h
    size = fromIntegral . B.length $ bs
    candidates :: [Word8]
    candidates = [0..255]

score :: B.ByteString -> Int
score _ = 0
