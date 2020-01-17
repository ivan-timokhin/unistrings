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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Memory.Unsafe
Description : Unsafe functions for working with underlying memory allocations
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Memory.Unsafe
  ( CountOf(CountOf, getCountOf)
  , ByteCount(ByteCount, getByteCount)
  , Primitive(inBytes, inElements, uncheckedIndexPtr, uncheckedReadPtr,
          uncheckedWritePtr, uncheckedIndexBytes, uncheckedReadBytes,
          uncheckedWriteBytes)
  , ForeignArray(ForeignArray, foreignArrayPtr, foreignArrayLength)
  , NativeArray(NativeArray, nativeArrayBytes)
  , NativeMutableArray(NativeMutableArray, nativeMutableArrayBytes)
  , uncheckedIndexNative
  , uncheckedReadNative
  , uncheckedWriteNative
  , nativeArrayLength
  , getNativeMutableArrayLength
  , MutableArray(uncheckedRead, uncheckedWrite)
  , Storage(Native, Foreign)
  , Array(NArray, FArray, getNArray, getFArray)
  , arrayLength
  , arrayToList
  , forgetArrayAllocator
  , allocatorCoercion
  , Default
  , Pinned
  , Unknown
  , AllocatorM(new)
  , Allocator(withAllocator)
  , Sing(SNative, SForeign)
  , storage
  ) where

import Control.Monad.ST (runST, stToIO)
import Data.Bits (Bits((.&.), complement, shiftL, shiftR), FiniteBits)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Traversable (for)
import Data.Type.Coercion (Coercion(Coercion), coerceWith)
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Exts
  ( ByteArray#
  , Int(I#)
  , IsList(Item, fromList, fromListN, toList)
  , MutableByteArray#
  , Ptr(Ptr)
  , RealWorld
  , byteArrayContents#
  , getSizeofMutableByteArray#
  , indexWord16Array#
  , indexWord16OffAddr#
  , indexWord32Array#
  , indexWord32OffAddr#
  , indexWord8Array#
  , indexWord8OffAddr#
  , newByteArray#
  , newPinnedByteArray#
  , readWord16Array#
  , readWord16OffAddr#
  , readWord32Array#
  , readWord32OffAddr#
  , readWord8Array#
  , readWord8OffAddr#
  , sizeofByteArray#
  , unsafeFreezeByteArray#
  , writeWord16Array#
  , writeWord16OffAddr#
  , writeWord32Array#
  , writeWord32OffAddr#
  , writeWord8Array#
  , writeWord8OffAddr#
  )
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents(PlainPtr))
import GHC.IO (IO(IO))
import GHC.ST (ST(ST))
import GHC.TypeLits (ErrorMessage((:$$:), (:<>:), ShowType, Text), TypeError)
import GHC.Word (Word16(W16#), Word32(W32#), Word8(W8#))
import System.IO.Unsafe (unsafeDupablePerformIO)

import Data.Unistring.Singletons (Sing, Known(sing))

newtype CountOf a =
  CountOf
    { getCountOf :: Int
    }
  deriving (Eq, Ord, Show, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits)

type role CountOf representational

newtype ByteCount =
  ByteCount
    { getByteCount :: Int
    }
  deriving (Eq, Ord, Show, Enum, Bounded, Num, Real, Integral)

checkOverflow ::
     Int
  -> CountOf a
  -> CountOf a
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
  uncheckedIndexPtr :: Ptr a -> CountOf a -> a
  uncheckedReadPtr :: Ptr a -> CountOf a -> IO a
  uncheckedWritePtr :: Ptr a -> CountOf a -> a -> IO ()
  uncheckedIndexBytes :: ByteArray# -> CountOf a -> a
  uncheckedReadBytes :: MutableByteArray# s -> CountOf a -> ST s a
  uncheckedWriteBytes :: MutableByteArray# s -> CountOf a -> a -> ST s ()

instance Primitive Word8 where
  inBytes = ByteCount . getCountOf . checkNegative
  inElements = CountOf . getByteCount
  uncheckedIndexPtr (Ptr ptr) (CountOf (I# ix)) =
    W8# (indexWord8OffAddr# ptr ix)
  uncheckedReadPtr (Ptr ptr) (CountOf (I# ix)) =
    IO $ \s ->
      case readWord8OffAddr# ptr ix s of
        (# s', w #) -> (# s', W8# w #)
  uncheckedWritePtr (Ptr ptr) (CountOf (I# ix)) (W8# w) =
    IO $ \s -> (# writeWord8OffAddr# ptr ix w s, () #)
  uncheckedIndexBytes bytes (CountOf (I# ix)) = W8# (indexWord8Array# bytes ix)
  uncheckedReadBytes bytes (CountOf (I# ix)) =
    ST $ \s ->
      case readWord8Array# bytes ix s of
        (# s', w #) -> (# s', W8# w #)
  uncheckedWriteBytes bytes (CountOf (I# ix)) (W8# w) =
    ST $ \s -> (# writeWord8Array# bytes ix w s, () #)

instance Primitive Word16 where
  inBytes = ByteCount . (`shiftL` 1) . getCountOf . checkOverflow 1
  inElements = CountOf . (`shiftR` 1) . getByteCount
  uncheckedIndexPtr (Ptr ptr) (CountOf (I# ix)) =
    W16# (indexWord16OffAddr# ptr ix)
  uncheckedReadPtr (Ptr ptr) (CountOf (I# ix)) =
    IO $ \s ->
      case readWord16OffAddr# ptr ix s of
        (# s', w #) -> (# s', W16# w #)
  uncheckedWritePtr (Ptr ptr) (CountOf (I# ix)) (W16# w) =
    IO $ \s -> (# writeWord16OffAddr# ptr ix w s, () #)
  uncheckedIndexBytes bytes (CountOf (I# ix)) =
    W16# (indexWord16Array# bytes ix)
  uncheckedReadBytes bytes (CountOf (I# ix)) =
    ST $ \s ->
      case readWord16Array# bytes ix s of
        (# s', w #) -> (# s', W16# w #)
  uncheckedWriteBytes bytes (CountOf (I# ix)) (W16# w) =
    ST $ \s -> (# writeWord16Array# bytes ix w s, () #)

instance Primitive Word32 where
  inBytes = ByteCount . (`shiftL` 2) . getCountOf . checkOverflow 2
  inElements = CountOf . (`shiftR` 2) . getByteCount
  uncheckedIndexPtr (Ptr ptr) (CountOf (I# ix)) =
    W32# (indexWord32OffAddr# ptr ix)
  uncheckedReadPtr (Ptr ptr) (CountOf (I# ix)) =
    IO $ \s ->
      case readWord32OffAddr# ptr ix s of
        (# s', w #) -> (# s', W32# w #)
  uncheckedWritePtr (Ptr ptr) (CountOf (I# ix)) (W32# w) =
    IO $ \s -> (# writeWord32OffAddr# ptr ix w s, () #)
  uncheckedIndexBytes bytes (CountOf (I# ix)) =
    W32# (indexWord32Array# bytes ix)
  uncheckedReadBytes bytes (CountOf (I# ix)) =
    ST $ \s ->
      case readWord32Array# bytes ix s of
        (# s', w #) -> (# s', W32# w #)
  uncheckedWriteBytes bytes (CountOf (I# ix)) (W32# w) =
    ST $ \s -> (# writeWord32Array# bytes ix w s, () #)

data ForeignArray a =
  ForeignArray
    { foreignArrayPtr :: {-# UNPACK #-}!(ForeignPtr a)
    , foreignArrayLength :: {-# UNPACK #-}!(CountOf a)
    }

type role ForeignArray representational

data NativeArray a =
  NativeArray
    { nativeArrayBytes :: ByteArray#
    }

type role NativeArray representational

data NativeMutableArray s a =
  NativeMutableArray
    { nativeMutableArrayBytes :: MutableByteArray# s
    }

type role NativeMutableArray nominal representational

uncheckedIndexNative :: Primitive a => NativeArray a -> CountOf a -> a
{-# INLINE uncheckedIndexNative #-}
uncheckedIndexNative NativeArray {nativeArrayBytes = bytes} =
  uncheckedIndexBytes bytes

uncheckedReadNative ::
     Primitive a => NativeMutableArray s a -> CountOf a -> ST s a
{-# INLINE uncheckedReadNative #-}
uncheckedReadNative NativeMutableArray {nativeMutableArrayBytes = bytes} =
  uncheckedReadBytes bytes

uncheckedWriteNative ::
     Primitive a => NativeMutableArray s a -> CountOf a -> a -> ST s ()
{-# INLINE uncheckedWriteNative #-}
uncheckedWriteNative NativeMutableArray {nativeMutableArrayBytes = bytes} =
  uncheckedWriteBytes bytes

nativeArrayLength :: Primitive a => NativeArray a -> CountOf a
{-# INLINE nativeArrayLength #-}
nativeArrayLength NativeArray {nativeArrayBytes = bytes} =
  inElements (sizeOfByteArray bytes)

getNativeMutableArrayLength ::
     Primitive a => NativeMutableArray s a -> ST s (CountOf a)
{-# INLINE getNativeMutableArrayLength #-}
getNativeMutableArrayLength NativeMutableArray {nativeMutableArrayBytes = bytes} =
  inElements <$> getSizeOfMutableByteArray bytes

sizeOfByteArray :: ByteArray# -> ByteCount
{-# INLINE sizeOfByteArray #-}
sizeOfByteArray ba = ByteCount (I# (sizeofByteArray# ba))

getSizeOfMutableByteArray :: MutableByteArray# s -> ST s ByteCount
{-# INLINE getSizeOfMutableByteArray #-}
getSizeOfMutableByteArray mba = ST $ \s ->
  case getSizeofMutableByteArray# mba s of
    (# s', n #) -> (# s', ByteCount (I# n) #)

class Monad m =>
      MutableArray arr m
  where
  uncheckedRead :: Primitive a => arr a -> CountOf a -> m a
  uncheckedWrite :: Primitive a => arr a -> CountOf a -> a -> m ()

instance MutableArray Ptr IO where
  uncheckedRead = uncheckedReadPtr
  uncheckedWrite = uncheckedWritePtr

instance MutableArray (NativeMutableArray s) (ST s) where
  uncheckedRead = uncheckedReadNative
  uncheckedWrite = uncheckedWriteNative

instance MutableArray (NativeMutableArray RealWorld) IO where
  uncheckedRead arr = stToIO . uncheckedReadNative arr
  uncheckedWrite arr ix = stToIO . uncheckedWriteNative arr ix

data Storage
  = Native
  | Foreign

data instance Sing (storage :: Storage) where
  SNative :: Sing 'Native
  SForeign :: Sing 'Foreign

instance Known 'Native where
  sing = SNative

instance Known 'Foreign where
  sing = SForeign

data family Array (storage :: Storage) allocator :: Type -> Type

newtype instance Array 'Native allocator a =
  NArray {getNArray :: NativeArray a}

newtype instance Array 'Foreign allocator a =
  FArray {getFArray :: ForeignArray a}

class MutableArray arr m => AllocatorM arr m | m -> arr where
  new :: Primitive a => CountOf a -> m (arr a)

newtype AllocatorT alloc (arr :: Type -> Type) m a =
  AllocatorT
    { runAllocatorT :: m a
    }
  deriving (Functor, Applicative, Monad, MutableArray arr)

data Default
data Pinned
data Unknown

instance AllocatorM (NativeMutableArray s) (AllocatorT Default (NativeMutableArray s) (ST s)) where
  new n =
    AllocatorT $
    ST $ \s ->
      case newByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(I# byteCount) = getByteCount $ inBytes n

instance AllocatorM (NativeMutableArray s) (AllocatorT Pinned (NativeMutableArray s) (ST s)) where
  new n =
    AllocatorT $
    ST $ \s ->
      case newPinnedByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(I# byteCount) = getByteCount $ inBytes n

instance AllocatorM (NativeMutableArray RealWorld) (AllocatorT Pinned (NativeMutableArray RealWorld) IO) where
  new = cast . new
    where
      cast :: AllocatorT s a (ST RealWorld) b -> AllocatorT s a IO b
      cast = AllocatorT . stToIO . runAllocatorT

class Allocator (storage :: Storage) alloc where
  withAllocator ::
       Primitive a
    => (forall m arr. AllocatorM arr m =>
                        m (arr a))
    -> Array storage alloc a

instance Allocator 'Native Default where
  withAllocator f = runST $ withNativeAllocator run f
    where
      run :: AllocatorT Default arr m (arr a) -> m (arr a)
      run = runAllocatorT

instance Allocator 'Native Pinned where
  withAllocator f = runST $ withNativeAllocator run f
    where
      run :: AllocatorT Pinned arr m (arr a) -> m (arr a)
      run = runAllocatorT

withNativeAllocator ::
     AllocatorM (NativeMutableArray s) n
  => (n (NativeMutableArray s a) -> ST s (NativeMutableArray s a))
  -> n (NativeMutableArray s a)
  -> ST s (Array 'Native alloc a)
{-# INLINE withNativeAllocator #-}
withNativeAllocator run f = do
  mutArr <- run f
  arr <- unsafeFreezeNative mutArr
  pure $ NArray arr

instance Allocator 'Foreign Pinned where
  withAllocator f =
    unsafeDupablePerformIO $ do
      mutArr <- run f
      arr <- unsafeFreezeNativeToForeign mutArr
      pure $ FArray arr
    where
      run :: AllocatorT Pinned arr m (arr a) -> m (arr a)
      run = runAllocatorT

instance TypeError ('ShowType Default
                    ':<>: 'Text " cannot be used to allocate "
                    ':<>: 'ShowType 'Foreign
                    ':<>: 'Text " arrays;"
                    ':$$: 'Text "Use "
                    ':<>: 'ShowType Pinned
                    ':<>: 'Text " instead") =>
         Allocator 'Foreign Default where
  withAllocator _ = error "unreachable"

instance TypeError ('ShowType Unknown
                    ':<>: 'Text " is not an allocator, but a placeholder meaning that the actual allocator is not known;"
                    ':$$: 'Text "Use "
                    ':<>: 'ShowType Default
                    ':<>: 'Text " or "
                    ':<>: 'ShowType Pinned
                    ':<>: 'Text " instead") =>
         Allocator storage Unknown where
  withAllocator _ = error "unreachable"

unsafeFreezeNative :: NativeMutableArray s a -> ST s (NativeArray a)
{-# INLINE unsafeFreezeNative #-}
unsafeFreezeNative NativeMutableArray {nativeMutableArrayBytes = mbytes} =
  ST $ \s ->
    case unsafeFreezeByteArray# mbytes s of
      (# s', ba #) -> (# s', NativeArray ba #)

unsafeFreezeNativeToForeign ::
     Primitive a => NativeMutableArray RealWorld a -> IO (ForeignArray a)
{-# INLINE unsafeFreezeNativeToForeign #-}
unsafeFreezeNativeToForeign nma@NativeMutableArray {nativeMutableArrayBytes = bytes} = do
  n <- stToIO $ getNativeMutableArrayLength nma
  IO $ \s ->
    case unsafeFreezeByteArray# bytes s of
      (# s', ba #) ->
        let fptr = ForeignPtr (byteArrayContents# ba) (PlainPtr bytes)
         in (# s'
             , ForeignArray {foreignArrayPtr = fptr, foreignArrayLength = n}#)

storage :: Known storage => Array storage alloc a -> Sing storage
{-# INLINE storage #-}
storage = const sing

instance (Allocator storage alloc, Primitive a, Known storage) =>
         IsList (Array storage alloc a) where
  type Item (Array storage alloc a) = a
  fromList xs = fromListN (length xs) xs
  fromListN n xs =
    withAllocator $ do
      arr <- new (CountOf n)
      for_ (zip [0 ..] xs) $ uncurry (uncheckedWrite arr)
      pure arr
  {-# INLINEABLE fromListN #-}
  toList = arrayToList
  {-# INLINE toList #-}

arrayToList :: (Known storage, Primitive a) => Array storage alloc a -> [a]
{-# INLINE arrayToList #-}
arrayToList arr =
  case storage arr of
    SNative ->
      flip map [0 .. (getCountOf $ nativeArrayLength (getNArray arr) - 1)] $ \i ->
        uncheckedIndexNative (getNArray arr) (CountOf i)
    SForeign ->
      unsafeDupablePerformIO $
      withForeignPtr (foreignArrayPtr (getFArray arr)) $ \ptr ->
        for [0 .. foreignArrayLength (getFArray arr) - 1] $ \i ->
          uncheckedReadPtr ptr i

arrayLength ::
     (Known storage, Primitive a) => Array storage alloc a -> CountOf a
{-# INLINEABLE arrayLength #-}
arrayLength arr =
  case storage arr of
    SNative -> nativeArrayLength (getNArray arr)
    SForeign -> foreignArrayLength (getFArray arr)

allocatorCoercion ::
     forall alloc1 alloc2 storage a. Known storage
  => Coercion (Array storage alloc1 a) (Array storage alloc2 a)
{-# INLINE allocatorCoercion #-}
allocatorCoercion =
  case sing @storage of
    SNative -> Coercion
    SForeign -> Coercion

forgetArrayAllocator ::
     forall alloc storage a. Known storage
  => Array storage alloc a
  -> Array storage Unknown a
{-# INLINE forgetArrayAllocator #-}
forgetArrayAllocator =
  coerceWith (allocatorCoercion @alloc @Unknown @storage @a)
