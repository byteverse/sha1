{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language UnliftedFFITypes #-}

module Hash.Sha1
  ( boundedBuilder
  ) where

import Control.Monad.ST (ST)
import Data.Bytes.Builder.Bounded as BB
import Data.Bytes.Builder.Bounded.Unsafe as BBU
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (ByteArray(..),MutableByteArray(..))
import GHC.Exts (Int(I#),Int#,MutableByteArray#,ByteArray#)
import GHC.IO (unsafeIOToST)

foreign import ccall unsafe "sha1.h hs_cryptohash_sha1_onepass"
  c_hash :: MutableByteArray# s -> Int# -> ByteArray# -> Int# -> Int# -> IO ()

performHash :: MutableByteArray s -> Int -> ByteArray -> Int -> Int -> ST s ()
performHash (MutableByteArray x) (I# a) (ByteArray y) (I# b) (I# c) =
  unsafeIOToST (c_hash x a y b c)

boundedBuilder :: Bytes -> BB.Builder 20
boundedBuilder (Bytes arr off len) = BBU.construct
  (\buf ix -> do
    performHash buf ix arr off len
    pure (ix + 20)
  )
