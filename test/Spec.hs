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
  Gen.list (Range.linear 1 100) (Gen.element $ symbolsToWord8 b64Symbols)
  where
    b64Symbols = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['/', '+']


forAllHex = forAll $
  Gen.filter (\hex -> length hex `mod` 6 == 0) $
  Gen.list (Range.linear 1 100) (Gen.element $ symbolsToWord8 hexSymbols)
  where
    hexSymbols = ['0'..'9'] ++ ['A'..'F']


prop_groupByN :: Property
prop_groupByN =
    property $ do
      n <- forAll $ Gen.int (Range.linear 1 8)
      words <-
        forAll $
        Gen.filter (\words -> length words `mod` (n * 8) == 0) $
        Gen.list (Range.linear 1 100) (Gen.word8 $ Range.linear 0 255)

      let bitStream = map (BitChunk 8) words
      let groups = map (BitChunk n) $ groupByN n bitStream
      toList groups === toList bitStream


prop_hex :: Property
prop_hex =
    property $ do
      hex <- forAllHex
      let packed = B.pack hex
      packed === (bitsToHex . hexToBits $ packed)


prop_base64 :: Property
prop_base64 =
    property $ do
      b64 <- forAllB64
      let packed = B.pack b64
      packed === (bitsToB64 . b64ToBits $ packed)


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
