{-
Copyright 2020 Ivan Timokhin

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Memory.Primitive.Class.Unsafe
Description : Unsafe API of the Primitive class
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Memory.Primitive.Class.Unsafe
  ( Primitive(inBytes, inElements, uncheckedIndexPtr, uncheckedReadPtr,
          uncheckedWritePtr, uncheckedIndexBytes, uncheckedReadBytes,
          uncheckedWriteBytes)
  ) where

import Data.Bits ((.&.), complement, shiftL, shiftR)
import qualified GHC.Exts as E
import GHC.IO (IO(IO))
import GHC.ST (ST(ST))
import GHC.Word (Word16(W16#), Word32(W32#), Word8(W8#))

import Data.Unistring.Memory.Count
  ( ByteCount(ByteCount, getByteCount)
  , CountOf(CountOf, getCountOf)
  )

checkOverflow :: Int -> CountOf a -> CountOf a
{-# INLINE checkOverflow #-}
-- The mask is basically a complement of maxBound `div` sizeof @a,
-- which consists of all 1s up to some senior bit.
--
-- If the number is negative, it will have a sign bit set (which is
-- included in the mask, since maxBound is positive), and the test
-- will fail.
--
-- If it is positive, the condition we're interested in is n <=
-- maxBound `div` sizeof @a, which, for the particular case of all-1s
-- upper bound, is equivalent to testing that it has no even more
-- senior bits set.
checkOverflow nbits c@(CountOf n)
  | n .&. mask == 0 = c
  | otherwise = errorWithoutStackTrace $ "Invalid element count: " ++ show c
  where
    mask :: Int
    mask = complement $ maxBound `shiftR` nbits

checkNegative :: CountOf Word8 -> CountOf Word8
{-# INLINE checkNegative #-}
checkNegative c
  | c < 0 = errorWithoutStackTrace $ "Invalid element count: " ++ show c
  | otherwise = c

class Primitive a where
  inBytes :: CountOf a -> ByteCount
  inElements :: ByteCount -> CountOf a
  uncheckedIndexPtr :: E.Ptr a -> CountOf a -> a
  uncheckedReadPtr :: E.Ptr a -> CountOf a -> IO a
  uncheckedWritePtr :: E.Ptr a -> CountOf a -> a -> IO ()
  uncheckedIndexBytes :: E.ByteArray# -> CountOf a -> a
  uncheckedReadBytes :: E.MutableByteArray# s -> CountOf a -> ST s a
  uncheckedWriteBytes :: E.MutableByteArray# s -> CountOf a -> a -> ST s ()

instance Primitive Word8 where
  inBytes = ByteCount . getCountOf . checkNegative
  inElements = CountOf . getByteCount
  uncheckedIndexPtr (E.Ptr ptr) (CountOf (E.I# ix)) =
    W8# (E.indexWord8OffAddr# ptr ix)
  uncheckedReadPtr (E.Ptr ptr) (CountOf (E.I# ix)) =
    IO $ \s ->
      case E.readWord8OffAddr# ptr ix s of
        (# s', w #) -> (# s', W8# w #)
  uncheckedWritePtr (E.Ptr ptr) (CountOf (E.I# ix)) (W8# w) =
    IO $ \s -> (# E.writeWord8OffAddr# ptr ix w s, () #)
  uncheckedIndexBytes bytes (CountOf (E.I# ix)) =
    W8# (E.indexWord8Array# bytes ix)
  uncheckedReadBytes bytes (CountOf (E.I# ix)) =
    ST $ \s ->
      case E.readWord8Array# bytes ix s of
        (# s', w #) -> (# s', W8# w #)
  uncheckedWriteBytes bytes (CountOf (E.I# ix)) (W8# w) =
    ST $ \s -> (# E.writeWord8Array# bytes ix w s, () #)

instance Primitive Word16 where
  inBytes = ByteCount . (`shiftL` 1) . getCountOf . checkOverflow 1
  inElements = CountOf . (`shiftR` 1) . getByteCount
  uncheckedIndexPtr (E.Ptr ptr) (CountOf (E.I# ix)) =
    W16# (E.indexWord16OffAddr# ptr ix)
  uncheckedReadPtr (E.Ptr ptr) (CountOf (E.I# ix)) =
    IO $ \s ->
      case E.readWord16OffAddr# ptr ix s of
        (# s', w #) -> (# s', W16# w #)
  uncheckedWritePtr (E.Ptr ptr) (CountOf (E.I# ix)) (W16# w) =
    IO $ \s -> (# E.writeWord16OffAddr# ptr ix w s, () #)
  uncheckedIndexBytes bytes (CountOf (E.I# ix)) =
    W16# (E.indexWord16Array# bytes ix)
  uncheckedReadBytes bytes (CountOf (E.I# ix)) =
    ST $ \s ->
      case E.readWord16Array# bytes ix s of
        (# s', w #) -> (# s', W16# w #)
  uncheckedWriteBytes bytes (CountOf (E.I# ix)) (W16# w) =
    ST $ \s -> (# E.writeWord16Array# bytes ix w s, () #)

instance Primitive Word32 where
  inBytes = ByteCount . (`shiftL` 2) . getCountOf . checkOverflow 2
  inElements = CountOf . (`shiftR` 2) . getByteCount
  uncheckedIndexPtr (E.Ptr ptr) (CountOf (E.I# ix)) =
    W32# (E.indexWord32OffAddr# ptr ix)
  uncheckedReadPtr (E.Ptr ptr) (CountOf (E.I# ix)) =
    IO $ \s ->
      case E.readWord32OffAddr# ptr ix s of
        (# s', w #) -> (# s', W32# w #)
  uncheckedWritePtr (E.Ptr ptr) (CountOf (E.I# ix)) (W32# w) =
    IO $ \s -> (# E.writeWord32OffAddr# ptr ix w s, () #)
  uncheckedIndexBytes bytes (CountOf (E.I# ix)) =
    W32# (E.indexWord32Array# bytes ix)
  uncheckedReadBytes bytes (CountOf (E.I# ix)) =
    ST $ \s ->
      case E.readWord32Array# bytes ix s of
        (# s', w #) -> (# s', W32# w #)
  uncheckedWriteBytes bytes (CountOf (E.I# ix)) (W32# w) =
    ST $ \s -> (# E.writeWord32Array# bytes ix w s, () #)
