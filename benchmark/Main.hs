{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B
import Criterion.Main
import Lib

import Data.ByteString.Base64.Lazy as B64Lazy


hexInf  :: B.ByteString
hexInf  = B.cycle "0123456789abcdef"
hex1k   :: B.ByteString
hex1k   = B.take 1000 hexInf
hex10k  :: B.ByteString
hex10k  = B.take 10000 hexInf
hex100k :: B.ByteString
hex100k = B.take 100000 hexInf
hex1M   :: B.ByteString
hex1M   = B.take 1000000 hexInf

b64Inf  :: B.ByteString
b64Inf  = B.cycle "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
b64_1k   :: B.ByteString
b64_1k   = B.take 1000 b64Inf
b64_10k  :: B.ByteString
b64_10k  = B.take 10000 b64Inf
b64_100k :: B.ByteString
b64_100k = B.take 100000 b64Inf
b64_1M   :: B.ByteString
b64_1M   = B.take 1000000 b64Inf


main :: IO ()
main = defaultMain
  [ bgroup "hex2base64"
      [ bench "1k"   $ nf hex2base64 hex1k
      , bench "10k"  $ nf hex2base64 hex10k
      , bench "100k" $ nf hex2base64 hex100k
      , bench "1M"   $ nf hex2base64 hex1M
      ]
  , bgroup "hex"
      [ bench "1k"   $ nf hex hex1k
      , bench "10k"  $ nf hex hex10k
      , bench "100k" $ nf hex hex100k
      , bench "1M"   $ nf hex hex1M
      ]
  , bgroup "base64"
      [ bench "1k"   $ nf b64 b64_1k
      , bench "10k"  $ nf b64 b64_10k
      , bench "100k" $ nf b64 b64_100k
      , bench "1M"   $ nf b64 b64_1M
      ]
  , bgroup "base64_ref"
      [ bench "1k"   $ nf B64Lazy.encode b64_1k
      , bench "10k"  $ nf B64Lazy.encode b64_10k
      , bench "100k" $ nf B64Lazy.encode b64_100k
      , bench "1M"   $ nf B64Lazy.encode b64_1M
      ]
  ]
