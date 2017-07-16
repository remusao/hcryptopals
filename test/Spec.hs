{-# LANGUAGE TemplateHaskell #-}

import Data.Char
import Data.Word

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.ByteString.Lazy as B

import Lib


symbolsToWord8 :: String -> [Word8]
symbolsToWord8 = map (fromIntegral . ord)


forAllB64 = forAll $
  Gen.filter (\b64 -> length b64 `mod` 3 == 0) $
  Gen.list (Range.linear 1 100) (Gen.element $ symbolsToWord8 b64Symbols)
  where
    b64Symbols = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['/', '+']


forAllHex = forAll $
  Gen.filter (\hex -> length hex `mod` 6 == 0) $
  Gen.list (Range.linear 1 100) (Gen.element $ symbolsToWord8 hexSymbols)
  where
    hexSymbols = ['0'..'9'] ++ ['a'..'f']


prop_groupByN :: Property
prop_groupByN =
    property $ do
      n <- forAll $ Gen.int (Range.linear 1 8)
      words <-
        forAll $
        Gen.filter (\words -> length words `mod` (n * 8) == 0) $
        Gen.list (Range.linear 1 100) (Gen.word8 $ Range.linear 0 255)

      let bitStream = map (BitChunk 8) words
      let bitStream2 = map (BitChunk n) $ groupByN n bitStream
      let bitStream3 = map (BitChunk 8) $ groupByN 8 bitStream2
      bitStream === bitStream3
      toList bitStream3 === toList bitStream


prop_hex :: Property
prop_hex =
    property $ do
      str <- forAllHex
      let packed = B.pack str
      packed === (unhex . hex $ packed)


prop_base64 :: Property
prop_base64 =
    property $ do
      str <- forAllB64
      let packed = B.pack str
      packed === (unb64 . b64 $ packed)


prop_hex2b64 :: Property
prop_hex2b64 =
    property $ do
      hex <- forAllHex
      let packed = B.pack hex
      let b64 = hex2base64 packed
      let bits = b64ToBits b64
      let hex2 = bitsToHex bits
      packed === hex2


main :: IO Bool
main =
  checkParallel $$(discover)
