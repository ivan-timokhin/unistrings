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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
  , Allocator(withAllocator, adopt)
  , Sing(SNative, SForeign)
  , allocator
  , storage
  ) where

import Control.Monad.ST (runST, stToIO)
import Data.Bits (Bits((.&.), complement, shiftL, shiftR), FiniteBits)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Traversable (for)
import Data.Type.Coercion (Coercion(Coercion), coerceWith)
import Foreign.ForeignPtr (withForeignPtr)
import qualified GHC.Exts as E
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents(PlainPtr))
import GHC.IO (IO(IO))
import GHC.ST (ST(ST))
import GHC.TypeLits (ErrorMessage((:$$:), (:<>:), ShowType, Text), TypeError)
import GHC.Word (Word16(W16#), Word32(W32#), Word8(W8#))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Typeable (Typeable)
import Data.Type.Equality ((:~:)(Refl), testEquality)
import Foreign.C.Types (CInt(CInt), CSize(CSize))

import Data.Unistring.Singletons
  ( Decided(Disproven, Proven)
  , Known(sing)
  , SDecide(decideEq)
  , Sing
  )
import Data.Unistring.Compat.Typeable (TypeRep, typeRep)

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
  uncheckedIndexBytes bytes (CountOf (E.I# ix)) = W8# (E.indexWord8Array# bytes ix)
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

data ForeignArray a =
  ForeignArray
    { foreignArrayPtr :: {-# UNPACK #-}!(ForeignPtr a)
    , foreignArrayLength :: {-# UNPACK #-}!(CountOf a)
    }

type role ForeignArray representational

data NativeArray a =
  NativeArray
    { nativeArrayBytes :: E.ByteArray#
    }

type role NativeArray representational

data NativeMutableArray s a =
  NativeMutableArray
    { nativeMutableArrayBytes :: E.MutableByteArray# s
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

sizeOfByteArray :: E.ByteArray# -> ByteCount
{-# INLINE sizeOfByteArray #-}
sizeOfByteArray ba = ByteCount (E.I# (E.sizeofByteArray# ba))

getSizeOfMutableByteArray :: E.MutableByteArray# s -> ST s ByteCount
{-# INLINE getSizeOfMutableByteArray #-}
getSizeOfMutableByteArray mba = ST $ \s ->
  case E.getSizeofMutableByteArray# mba s of
    (# s', n #) -> (# s', ByteCount (E.I# n) #)

class Monad m =>
      MutableArray arr m
  where
  uncheckedRead :: Primitive a => arr a -> CountOf a -> m a
  uncheckedWrite :: Primitive a => arr a -> CountOf a -> a -> m ()

instance MutableArray E.Ptr IO where
  uncheckedRead = uncheckedReadPtr
  uncheckedWrite = uncheckedWritePtr

instance MutableArray (NativeMutableArray s) (ST s) where
  uncheckedRead = uncheckedReadNative
  uncheckedWrite = uncheckedWriteNative

instance MutableArray (NativeMutableArray E.RealWorld) IO where
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

instance SDecide Storage where
  decideEq SNative SNative = Proven Refl
  decideEq SForeign SForeign = Proven Refl
  decideEq SNative SForeign = Disproven $ \case {}
  decideEq SForeign SNative = Disproven $ \case {}

data family Array (storage :: Storage) allocator :: Type -> Type

newtype instance Array 'Native allocator a =
  NArray {getNArray :: NativeArray a}

newtype instance Array 'Foreign allocator a =
  FArray {getFArray :: ForeignArray a}

instance (Known storage, Primitive a) => Eq (Array storage allocator a) where
  l == r =
    case storage l of
      SNative ->
        let !(NArray (NativeArray l#)) = l
            !(NArray (NativeArray r#)) = r
            !llen# = E.sizeofByteArray# l#
            !rlen# = E.sizeofByteArray# r#
         in E.isTrue# (llen# E.==# rlen#) &&
            E.isTrue# (E.compareByteArrays# l# 0# r# 0# llen# E.==# 0#)
      SForeign ->
        let !(FArray (ForeignArray lfptr llen)) = l
            !(FArray (ForeignArray rfptr rlen)) = r
         in llen == rlen &&
            unsafeDupablePerformIO
              (withForeignPtr lfptr $ \lptr ->
                 withForeignPtr rfptr $ \rptr -> do
                   diff <- memcmp lptr rptr (inBytes llen)
                   pure (diff == 0))

-- Allowed to have false negatives, but not false positives
isDefinitelyPinned :: Typeable allocator => Array 'Native allocator a -> Bool
{-# INLINE isDefinitelyPinned #-}
#if MIN_VERSION_base(4, 10, 0)
-- On GHC >= 8.2/base >= 4.10, there's a isByteArrayPinned# primop,
-- through which we can just ask the RTS if the array is pinned.
isDefinitelyPinned (NArray (NativeArray ba#)) =
  E.isTrue# (E.isByteArrayPinned# ba#)
#else
-- On GHC 8.0/base 4.9, the primop is absent, so we resort to the next
-- best thing—seeing if the array was allocated via Pinned allocator.
isDefinitelyPinned array =
  -- Not using isJust, because it's used nowhere else in this module,
  -- so I would have to add a CPP'd import, which is one thing I
  -- like less than CPP'd code.
  case testEquality (allocator array) (typeRep @Pinned) of
    Just Refl -> True
    Nothing -> False
#endif

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
      case E.newByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(E.I# byteCount) = getByteCount $ inBytes n

instance AllocatorM (NativeMutableArray s) (AllocatorT Pinned (NativeMutableArray s) (ST s)) where
  new n =
    AllocatorT $
    ST $ \s ->
      case E.newPinnedByteArray# byteCount s of
        (# s', mba #) -> (# s', NativeMutableArray mba #)
    where
      !(E.I# byteCount) = getByteCount $ inBytes n

instance AllocatorM (NativeMutableArray E.RealWorld) (AllocatorT Pinned (NativeMutableArray E.RealWorld) IO) where
  new = cast . new
    where
      cast :: AllocatorT s a (ST E.RealWorld) b -> AllocatorT s a IO b
      cast = AllocatorT . stToIO . runAllocatorT

class (Known storage, Typeable alloc) =>
      Allocator (storage :: Storage) alloc
  where
  withAllocator ::
       Primitive a
    => (forall m arr. AllocatorM arr m =>
                        m (arr a))
    -> Array storage alloc a
  adopt ::
       (Typeable alloc', Known storage', Primitive a)
    => Array storage' alloc' a
    -> Maybe (Array storage alloc a)
  adopt arr = do
    Refl <- testEquality (allocator arr) (typeRep @alloc)
    Refl <- testEquality (storage arr) (sing @storage)
    Just arr

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
  adopt array
    | SNative <- storage array
    , isDefinitelyPinned array
    , (NArray (NativeArray ba#)) <- array
    = Just (NArray (NativeArray ba#))
    | otherwise = Nothing

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
  adopt array =
    case storage array of
      SForeign ->
        case testEquality (allocator array) (typeRep @Pinned) of
          Just Refl -> Just array
          Nothing -> Nothing
      SNative
        | (NArray narr@(NativeArray ba#)) <- array
        , isDefinitelyPinned array ->
          Just $
          FArray $
          ForeignArray
            (ForeignPtr (E.byteArrayContents# ba#) (PlainPtr (E.unsafeCoerce# ba#)))
            (nativeArrayLength narr)
            -- mallocForeignPtrBytes in GHC.ForeignPtr has exactly the
            -- same unsafeCoerce#, but in opposite direction, so I
            -- assume this is safe-ish.  Especially since nobody's
            -- going to /access/ this byte array, it's just there to
            -- keep it alive.
        | otherwise -> Nothing

instance TypeError ('ShowType Default
                    ':<>: 'Text " cannot be used to allocate "
                    ':<>: 'ShowType 'Foreign
                    ':<>: 'Text " arrays;"
                    ':$$: 'Text "Use "
                    ':<>: 'ShowType Pinned
                    ':<>: 'Text " instead") =>
         Allocator 'Foreign Default where
  withAllocator _ = error "unreachable"

instance (TypeError ('ShowType Unknown
                     ':<>: 'Text " is not an allocator, but a placeholder meaning that the actual allocator is not known;"
                     ':$$: 'Text "Use "
                     ':<>: 'ShowType Default
                     ':<>: 'Text " or "
                     ':<>: 'ShowType Pinned
                     ':<>: 'Text " instead")
         , Known storage) =>
         Allocator storage Unknown where
  withAllocator _ = error "unreachable"

unsafeFreezeNative :: NativeMutableArray s a -> ST s (NativeArray a)
{-# INLINE unsafeFreezeNative #-}
unsafeFreezeNative NativeMutableArray {nativeMutableArrayBytes = mbytes} =
  ST $ \s ->
    case E.unsafeFreezeByteArray# mbytes s of
      (# s', ba #) -> (# s', NativeArray ba #)

unsafeFreezeNativeToForeign ::
     Primitive a => NativeMutableArray E.RealWorld a -> IO (ForeignArray a)
{-# INLINE unsafeFreezeNativeToForeign #-}
unsafeFreezeNativeToForeign nma@NativeMutableArray {nativeMutableArrayBytes = bytes} = do
  n <- stToIO $ getNativeMutableArrayLength nma
  IO $ \s ->
    case E.unsafeFreezeByteArray# bytes s of
      (# s', ba #) ->
        let fptr = ForeignPtr (E.byteArrayContents# ba) (PlainPtr bytes)
         in (# s'
             , ForeignArray {foreignArrayPtr = fptr, foreignArrayLength = n}#)

allocator :: Typeable alloc => Array storage alloc a -> TypeRep alloc
{-# INLINE allocator #-}
allocator = const typeRep

storage :: Known storage => Array storage alloc a -> Sing storage
{-# INLINE storage #-}
storage = const sing

instance (Allocator storage alloc, Primitive a) =>
         E.IsList (Array storage alloc a) where
  type Item (Array storage alloc a) = a
  fromList xs = E.fromListN (length xs) xs
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

memcmp :: E.Ptr a -> E.Ptr a -> ByteCount -> IO Int
{-# INLINE memcmp #-}
memcmp lp rp len = fromIntegral <$> c_memcmp lp rp (fromIntegral len)

-- See note ‘Unsafe FFI’
foreign import ccall unsafe "string.h memcmp" c_memcmp
  :: E.Ptr a -> E.Ptr a -> CSize -> IO CInt
