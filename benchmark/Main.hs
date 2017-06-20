{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as B
import Criterion.Main
import Lib


hexInf  :: String
hexInf  = cycle $ ['0'..'9'] ++ ['A'..'F']
hex1k   :: B.ByteString
hex1k   = B.pack $! take 1000 hexInf
hex10k  :: B.ByteString
hex10k  = B.pack $! take 10000 hexInf
hex100k :: B.ByteString
hex100k = B.pack $! take 100000 hexInf
hex1M   :: B.ByteString
hex1M   = B.pack $! take 1000000 hexInf
hex10M  :: B.ByteString
hex10M  = B.pack $! take 10000000 hexInf


main :: IO ()
main = defaultMain
  [ bgroup "hex2base64"
      [ bench "1k"   $ nf hex2base64 hex1k
      , bench "10k"  $ nf hex2base64 hex10k
      , bench "100k" $ nf hex2base64 hex100k
      , bench "1M"   $ nf hex2base64 hex1M
      , bench "10M"  $ nf hex2base64 hex10M
      ]
  ]
