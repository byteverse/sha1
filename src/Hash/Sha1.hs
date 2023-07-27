{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnliftedFFITypes #-}

module Hash.Sha1
  ( boundedBuilder
  , byteArrayN
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Builder.Bounded as BB
import Data.Bytes.Builder.Bounded.Unsafe as BBU
import Data.Bytes.Types (Bytes(Bytes),ByteArrayN(ByteArrayN))
import Data.Primitive (ByteArray(..),MutableByteArray(..))
import GHC.Exts (Int(I#),Int#,MutableByteArray#,ByteArray#)
import GHC.IO (unsafeIOToST)

import qualified Data.Primitive as PM

foreign import ccall unsafe "sha1.h hs_cryptohash_sha1_onepass"
  c_hash :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> IO ()

performHash :: MutableByteArray s -> Int -> ByteArray -> Int -> Int -> ST s ()
performHash (MutableByteArray x) (I# a) (ByteArray y) (I# b) (I# c) =
  unsafeIOToST (c_hash x a y b c)

-- | Hash the byte sequence, returning the result as a builder.
boundedBuilder :: Bytes -> BB.Builder 20
boundedBuilder (Bytes arr off len) = BBU.construct
  (\buf ix -> do
    performHash buf ix arr off len
    pure (ix + 20)
  )

-- | Hash the byte sequence, returning the result as a byte array
-- known to have exactly 20 bytes.
byteArrayN :: Bytes -> ByteArrayN 20
byteArrayN (Bytes arr off len) = ByteArrayN $ runByteArrayST $ do
  dst <- PM.newByteArray 20
  performHash dst 0 arr off len
  PM.unsafeFreezeByteArray dst
