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
{-# LANGUAGE GADTs #-}
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
  , Primitive(inBytes, uncheckedIndexPtr, uncheckedReadPtr,
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
  , Array (NArray, FArray, getNArray, getFArray)
  , arrayLength
  , Default
  , Pinned
  , AllocatorM (new)
  , Allocator (withAllocator)
  , Sing(SNative, SForeign)
  , storageSing
  ) where

import GHC.Exts
  ( ByteArray#
  , Int(I#)
  , MutableByteArray#
  , Ptr(Ptr)
  , indexWord16Array#
  , indexWord16OffAddr#
  , indexWord32Array#
  , indexWord32OffAddr#
  , indexWord8Array#
  , indexWord8OffAddr#
  , readWord16Array#
  , readWord16OffAddr#
  , readWord32Array#
  , readWord32OffAddr#
  , readWord8Array#
  , readWord8OffAddr#
  , writeWord16Array#
  , writeWord16OffAddr#
  , writeWord32Array#
  , writeWord32OffAddr#
  , writeWord8Array#
  , writeWord8OffAddr#
  , newByteArray#
  , unsafeFreezeByteArray#
  , sizeofByteArray#
  , getSizeofMutableByteArray#
  , byteArrayContents#
  , newPinnedByteArray#
  , RealWorld
  , IsList(Item, fromList, toList, fromListN)
  )
import GHC.IO (IO(IO))
import GHC.ST (ST(ST))
import Control.Monad.ST (runST, stToIO)
import GHC.Word (Word16(W16#), Word32(W32#), Word8(W8#))
import Data.Kind (Type)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents (PlainPtr))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Foldable (for_)
import Data.Traversable (for)
import Foreign.ForeignPtr (withForeignPtr)

import Data.Unistring.Singletons (Sing, Known(sing))

newtype CountOf a =
  CountOf
    { getCountOf :: Int
    }
  deriving (Eq, Ord, Show, Enum, Bounded, Num, Real, Integral)

type role CountOf representational

newtype ByteCount =
  ByteCount
    { getByteCount :: Int
    }
  deriving (Eq, Ord, Show, Enum, Bounded, Num, Real, Integral)

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
  inBytes = ByteCount . getCountOf
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
  inBytes = ByteCount . (* 2) . getCountOf
  inElements = CountOf . (`div` 2) . getByteCount
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
  inBytes = ByteCount . (* 4) . getCountOf
  inElements = CountOf . (`div` 4) . getByteCount
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

data family Array allocator (storage :: Storage) :: Type -> Type

newtype instance Array allocator 'Native a =
  NArray {getNArray :: NativeArray a}

newtype instance Array allocator 'Foreign a =
  FArray {getFArray :: ForeignArray a}

class MutableArray arr m => AllocatorM alloc arr m | m -> arr alloc where
  new :: Primitive a => CountOf a -> m (arr a)

newtype AllocatorT alloc (arr :: Type -> Type) m a =
  AllocatorT
    { runAllocatorT :: m a
    }
  deriving (Functor, Applicative, Monad, MutableArray arr)

data Default
data Pinned

instance AllocatorM Default (NativeMutableArray s) (AllocatorT Default (NativeMutableArray s) (ST s)) where
  new n =
    AllocatorT $
    ST $ \s ->
      case newByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(I# byteCount) = getByteCount $ inBytes n

instance AllocatorM Pinned (NativeMutableArray s) (AllocatorT Pinned (NativeMutableArray s) (ST s)) where
  new n =
    AllocatorT $
    ST $ \s ->
      case newPinnedByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(I# byteCount) = getByteCount $ inBytes n

instance AllocatorM Pinned (NativeMutableArray RealWorld) (AllocatorT Pinned (NativeMutableArray RealWorld) IO) where
  new = cast . new
    where
      cast :: AllocatorT s a (ST RealWorld) b -> AllocatorT s a IO b
      cast = AllocatorT . stToIO . runAllocatorT

class Allocator (storage :: Storage) alloc where
  withAllocator ::
       Primitive a
    => (forall m arr. AllocatorM alloc arr m =>
                        m (arr a))
    -> Array alloc storage a

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
     AllocatorM alloc (NativeMutableArray s) n
  => (n (NativeMutableArray s a) -> ST s (NativeMutableArray s a))
  -> n (NativeMutableArray s a)
  -> ST s (Array alloc 'Native a)
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

storageSing :: Known storage => Array alloc storage a -> Sing storage
{-# INLINE storageSing #-}
storageSing = const sing

instance (Allocator storage alloc, Primitive a, Known storage) =>
         IsList (Array alloc storage a) where
  type Item (Array alloc storage a) = a
  fromList xs = fromListN (length xs) xs
  fromListN n xs =
    withAllocator $ do
      arr <- new (CountOf n)
      for_ (zip [0 ..] xs) $ uncurry (uncheckedWrite arr)
      pure arr
  toList arr =
    case storageSing arr of
      SNative ->
        flip map [0 .. (nativeArrayLength (getNArray arr) - 1)] $ \i ->
          uncheckedIndexNative (getNArray arr) i
      SForeign ->
        unsafeDupablePerformIO $
        withForeignPtr (foreignArrayPtr (getFArray arr)) $ \ptr ->
          for [0 .. foreignArrayLength (getFArray arr) - 1] $ \i ->
            uncheckedReadPtr ptr i
  {-# INLINE toList #-}

arrayLength ::
     (Known storage, Primitive a) => Array alloc storage a -> CountOf a
{-# INLINE arrayLength #-}
arrayLength arr =
  case storageSing arr of
    SNative -> nativeArrayLength (getNArray arr)
    SForeign -> foreignArrayLength (getFArray arr)
