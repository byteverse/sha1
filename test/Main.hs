{-# language BangPatterns #-}

import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Numeric (showHex)

import qualified Arithmetic.Nat as Nat
import qualified Data.Bytes.Builder.Bounded as BB
import qualified Data.Bytes as Bytes
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Hash.Sha1 as Sha1

main :: IO ()
main = do
  putStrLn "Hashing: theoceanscovertheearth"
  putStr "Expected: "
  printHash expected
  putStr "Got:      "
  printHash actual
  if actual == expected
    then putStrLn "Success"
    else fail "Did not match"

printHash :: ByteArray -> IO ()
printHash !b = putStr (go 0) where
  go !ix = if ix < 20
    then let val = PM.indexByteArray b ix :: Word8 in
      if val < 16
        then '0' : showHex val (go (ix + 1))
        else showHex val (go (ix + 1))
    else "\n"

actual :: ByteArray
actual = BB.run Nat.constant
  (Sha1.boundedBuilder
    (Bytes.unsafeDrop 5
      (Bytes.fromAsciiString "12345theoceanscovertheearth")
    )
  )

expected :: ByteArray
expected = Exts.fromList
  [ 0x2d, 0x4c, 0xbf, 0xa2
  , 0x04, 0xb2, 0x0a, 0xda
  , 0x06, 0xee, 0xbf, 0x8b
  , 0x2c, 0x22, 0x23, 0x0c
  , 0x51, 0xe7, 0x55, 0x8f
  ]
