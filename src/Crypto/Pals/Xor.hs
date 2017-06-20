
module Crypto.Pals.Xor where

import qualified Data.Bits as Bits
import qualified Data.ByteString.Lazy as B

xor :: B.ByteString -> B.ByteString -> Maybe B.ByteString
xor bs1 bs2
  | B.length bs1 /= B.length bs2 = Nothing
  | otherwise = Just . B.pack . B.zipWith Bits.xor bs1 $ bs2
